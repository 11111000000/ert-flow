;;; test-flow-core.el --- Core of sessions/config/log for test-flow  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Core layer extracted from the monolithic test-flow.el:
;; - sessions registry and data structure
;; - per-session configuration (conf get/set, dir-locals snapshot)
;; - project root resolution
;; - lightweight logging
;; - common utilities (string-trim, suite-of, normalize-command, nix currentSystem)
;;
;; Public wrappers:
;; - test-flow-project-root
;; - test-flow-session-list
;; - test-flow-session-name
;;
;; Notes:
;; - This file intentionally avoids UI/runner code to keep dependencies minimal.
;; - Some config reads reference defcustoms defined elsewhere (runner/panel/watch).
;;   We guard with `boundp' to avoid unbound-variable errors when only core is loaded.

;;; Code:

(require 'cl-lib)
(require 'gv)
(require 'project)
(eval-when-compile (require 'subr-x))

;;;###autoload
(defgroup test-flow-core nil
  "Core utilities for test-flow."
  :group 'test-flow)

;; Ensure the logging flag is always bound (defcustom may override later).
(defvar test-flow-log-enabled nil
  "Enable lightweight logging for debugging (bound in core for safety).")

;;;; Logging and diagnostics

(defun test-flow--log (fmt &rest args)
  "Log a debug message FMT with ARGS when `test-flow-log-enabled' is non-nil."
  (when (and (boundp 'test-flow-log-enabled) test-flow-log-enabled)
    (apply #'message (concat "[test-flow] " fmt) args)))

;;;; Utilities

(defun test-flow--string-trim (s)
  "Trim whitespace around string S."
  (when (stringp s)
    (string-trim s)))

(defun test-flow--suite-of (test-name)
  "Return suite name derived from TEST-NAME (prefix until first '/')."
  (let* ((s (or test-name ""))
         (idx (string-match "/" s)))
    (if (and idx (> idx 0))
        (substring s 0 idx)
      s)))

(defun test-flow--normalize-command (value)
  "Normalize command VALUE to a list suitable for `make-process' :command.

If VALUE is a list, return it as-is.
If VALUE is a string, wrap with shell runner."
  (cond
   ((and (listp value) (stringp (car value)))
    value)
   ((stringp value)
    (let ((sh (or (getenv "SHELL") "/bin/sh")))
      (list sh "-lc" value)))
   (t nil)))

(defun test-flow--nix-current-system ()
  "Best-effort guess of Nix currentSystem string (e.g., x86_64-linux, aarch64-darwin)."
  (let* ((cfg (or system-configuration ""))
         (os (cond
              ((eq system-type 'darwin) "darwin")
              ((memq system-type '(gnu/linux gnu gnu/kfreebsd)) "linux")
              ((eq system-type 'windows-nt) "windows")
              (t "linux")))
         (arch (cond
                ((string-match-p "\\`x86_64" cfg) "x86_64")
                ((or (string-match-p "aarch64" cfg)
                     (string-match-p "arm64" cfg)) "aarch64")
                ((string-match-p "\\`i[3-6]86" cfg) "i686")
                (t "x86_64"))))
    (format "%s-%s" arch os)))

;;;; Project root

(defun test-flow--project-root ()
  "Return current project root directory as a string, or `default-directory'."
  (let* ((proj (project-current nil default-directory))
         (res (if proj
                  (expand-file-name (project-root proj))
                (expand-file-name default-directory))))
    (test-flow--log "project-root: %s (proj=%s)" res (and proj (project-root proj)))
    res))

;;;; Sessions

(defvar test-flow--sessions (make-hash-table :test 'equal)
  "Registry of test-flow sessions keyed by project root (absolute path).")

(cl-defstruct test-flow--session
  root
  panel-buf-name
  details-buf-name
  last-raw-output
  last-stderr-output
  last-results
  last-summary
  process
  debounce-timer
  watch-enabled
  file-notify-handles
  config
  last-parser
  last-activity-at)

;; Per-session configuration

(defun test-flow--dir-locals-snapshot (root symbols)
  "Return alist (SYMBOL . VALUE) of dir-local values for SYMBOLS under ROOT.
Creates a temporary buffer visiting a file in ROOT to read .dir-locals."
  (let ((buf (generate-new-buffer " *test-flow dirlocals*")))
    (unwind-protect
        (with-current-buffer buf
          (setq default-directory (file-name-as-directory (expand-file-name root)))
          (setq-local buffer-file-name (expand-file-name "._ert_flow_probe_.el" default-directory))
          (hack-dir-local-variables)
          (mapcar (lambda (sym) (cons sym (buffer-local-value sym buf))) symbols))
      (kill-buffer buf))))

(defun test-flow--init-session-config (root)
  "Build initial configuration alist for session at ROOT.

Keys include:
  runner parser external-command external-failed-args-function
  watch-mode debounce-seconds watch-include-regexp watch-exclude-regexp
  file-notify-max-depth panel-side panel-width icons toolbar-style"
  (let* ((mapping '((runner                        . test-flow-runner)
                    (parser                        . test-flow-parser)
                    (external-command              . test-flow-external-command)
                    (external-failed-args-function . test-flow-external-failed-args-function)
                    (watch-mode                    . test-flow-watch-mode)
                    (debounce-seconds              . test-flow-debounce-seconds)
                    (watch-include-regexp          . test-flow-watch-include-regexp)
                    (watch-exclude-regexp          . test-flow-watch-exclude-regexp)
                    (file-notify-max-depth         . test-flow-file-notify-max-depth)
                    (panel-side                    . test-flow-panel-side)
                    (panel-width                   . test-flow-panel-width)
                    (icons                         . test-flow-icons)
                    (toolbar-style                 . test-flow-toolbar-style)))
         (symbols (mapcar #'cdr mapping))
         (dirvals (condition-case _ (test-flow--dir-locals-snapshot root symbols) (error nil)))
         (alist nil))
    (dolist (pair mapping)
      (let* ((key (car pair))
             (var (cdr pair))
             (val (cond
                   ((and dirvals (assq var dirvals)) (cdr (assq var dirvals)))
                   ((boundp var) (symbol-value var))
                   (t nil))))
        (push (cons key val) alist)))
    (nreverse alist)))

(defun test-flow--conf (sess key default)
  "Get per-session configuration value for KEY from SESS or DEFAULT."
  (let* ((cfg (and sess (test-flow--session-config sess)))
         (cell (assq key cfg)))
    (if cell (cdr cell) default)))

(defun test-flow--set-conf (sess key value)
  "Set per-session configuration KEY to VALUE in SESS."
  (let* ((cfg (or (test-flow--session-config sess) '()))
         (cell (assq key cfg)))
    (if cell
        (setcdr cell value)
      (push (cons key value) cfg))
    (setf (test-flow--session-config sess) cfg)))

(defun test-flow--get-last-parser (sess)
  "Safe accessor for session's last parser symbol."
  (or (condition-case nil
          (test-flow--session-last-parser sess)
        (error nil))
      (test-flow--conf sess 'last-parser nil)))

(defun test-flow--set-last-parser (sess value)
  "Safe setter for session's last parser symbol."
  (condition-case nil
      (setf (test-flow--session-last-parser sess) value)
    (error (test-flow--set-conf sess 'last-parser value)))
  value)

(defun test-flow--get-last-activity-at (sess)
  "Safe accessor for session's last activity time."
  (or (condition-case nil
          (test-flow--session-last-activity-at sess)
        (error nil))
      (test-flow--conf sess 'last-activity-at nil)))

(defun test-flow--set-last-activity-at (sess value)
  "Safe setter for session's last activity time."
  (condition-case nil
      (setf (test-flow--session-last-activity-at sess) value)
    (error (test-flow--set-conf sess 'last-activity-at value)))
  value)

(defun test-flow--touch-session (sess)
  "Mark SESS as active now (update last-activity timestamp)."
  (when sess
    (test-flow--set-last-activity-at sess (current-time))))

(defun test-flow--get-watch-enabled (sess)
  "Safe accessor for session's watch-enabled flag."
  (or (condition-case nil
          (test-flow--session-watch-enabled sess)
        (error nil))
      (test-flow--conf sess 'watch-enabled nil)))

(defun test-flow--set-watch-enabled (sess value)
  "Safe setter for session's watch-enabled flag."
  (condition-case nil
      (setf (test-flow--session-watch-enabled sess) value)
    (error nil))
  (test-flow--set-conf sess 'watch-enabled value)
  value)

(defun test-flow--get-last-summary (sess)
  "Safe accessor for session's last summary."
  (or (condition-case nil
          (test-flow--session-last-summary sess)
        (error nil))
      (test-flow--conf sess 'last-summary nil)))

(defun test-flow--set-last-summary (sess value)
  "Safe setter for session's last summary."
  (condition-case nil
      (setf (test-flow--session-last-summary sess) value)
    (error nil))
  (test-flow--set-conf sess 'last-summary value)
  value)

(defun test-flow--get-last-results (sess)
  "Safe accessor for session's last results."
  (or (condition-case nil
          (test-flow--session-last-results sess)
        (error nil))
      (test-flow--conf sess 'last-results nil)))

(defun test-flow--set-last-results (sess value)
  "Safe setter for session's last results."
  (condition-case nil
      (setf (test-flow--session-last-results sess) value)
    (error nil))
  (test-flow--set-conf sess 'last-results value)
  value)

(defun test-flow--get-last-raw-output (sess)
  "Safe accessor for session's last raw stdout."
  (or (condition-case nil
          (test-flow--session-last-raw-output sess)
        (error nil))
      (test-flow--conf sess 'last-raw-output nil)))

(defun test-flow--set-last-raw-output (sess value)
  "Safe setter for session's last raw stdout."
  (condition-case nil
      (setf (test-flow--session-last-raw-output sess) value)
    (error nil))
  (test-flow--set-conf sess 'last-raw-output value)
  value)

(defun test-flow--get-last-stderr-output (sess)
  "Safe accessor for session's last raw stderr."
  (or (condition-case nil
          (test-flow--session-last-stderr-output sess)
        (error nil))
      (test-flow--conf sess 'last-stderr-output nil)))

(defun test-flow--set-last-stderr-output (sess value)
  "Safe setter for session's last raw stderr."
  (condition-case nil
      (setf (test-flow--session-last-stderr-output sess) value)
    (error nil))
  (test-flow--set-conf sess 'last-stderr-output value)
  value)

(defun test-flow--get-process (sess)
  "Safe accessor for session's process."
  (or (condition-case nil
          (test-flow--session-process sess)
        (error nil))
      (test-flow--conf sess 'process nil)))

(defun test-flow--set-process (sess value)
  "Safe setter for session's process."
  (condition-case nil
      (setf (test-flow--session-process sess) value)
    (error nil))
  (test-flow--set-conf sess 'process value)
  value)

(defun test-flow--default-session-name (root)
  "Return default human-friendly session name for ROOT."
  (file-name-nondirectory (directory-file-name root)))

(defun test-flow--session-panel-name (root)
  "Return panel buffer name for project ROOT."
  (let ((name (if (boundp 'test-flow-session-naming-function)
                  (funcall test-flow-session-naming-function root)
                (test-flow--default-session-name root))))
    (format "*test-flow: %s*" name)))

(defun test-flow--session-details-name (root)
  "Return details buffer name for project ROOT."
  (let ((name (if (boundp 'test-flow-session-naming-function)
                  (funcall test-flow-session-naming-function root)
                (test-flow--default-session-name root))))
    (format "*test-flow: %s: details*" name)))

(defun test-flow--get-session (&optional root)
  "Get or create a session for ROOT (current project if nil)."
  (let* ((r (or root (test-flow--project-root)))
         (abs (file-name-as-directory (expand-file-name r))))
    ;; Robustness: ensure the global registry exists (may be reset by restart/let)
    (unless (and (boundp 'test-flow--sessions)
                 (hash-table-p test-flow--sessions))
      (setq test-flow--sessions (make-hash-table :test 'equal)))
    (or (gethash abs test-flow--sessions)
        (let* ((sess (make-test-flow--session
                      :root abs
                      :panel-buf-name (test-flow--session-panel-name abs)
                      :details-buf-name (test-flow--session-details-name abs)
                      :last-raw-output nil
                      :last-stderr-output nil
                      :last-results nil
                      :last-summary nil
                      :process nil
                      :debounce-timer nil
                      :watch-enabled nil
                      :file-notify-handles nil
                      :config (test-flow--init-session-config abs))))
          (puthash abs sess test-flow--sessions)
          (test-flow--log "session: created root=%s panel=%s" abs (test-flow--session-panel-buf-name sess))
          sess))))

(defun test-flow--session-list ()
  "Return a list of current sessions."
  (let (acc)
    (maphash (lambda (_ s) (push s acc)) test-flow--sessions)
    acc))

;;;; Public wrappers (stable API)

;;;###autoload
(defun test-flow-project-root ()
  "Return current project root."
  (interactive)
  (test-flow--project-root))

;;;###autoload
(defun test-flow-session-list ()
  "Return list of active sessions."
  (test-flow--session-list))

;;;###autoload
(defun test-flow-session-name (root)
  "Return human-friendly session name for ROOT."
  (test-flow--default-session-name root))

(provide 'test-flow-core)
;;; test-flow-core.el ends here
