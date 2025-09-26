;;; test-flow-watch.el --- Watcher facade for test-flow  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Provides watch toggle and future watcher-related configuration.
;; Currently delegates to the monolithic implementation.

;;; Code:

(eval-when-compile (require 'subr-x))
(require 'cl-lib)
(require 'filenotify)
(require 'test-flow-core)

;;;###autoload
(defgroup test-flow-watch nil
  "Automatic test re-run (watch) for test-flow."
  :group 'test-flow)

;; Watcher options (moved here from monolith to avoid void-variable in lexical-binding)
(defcustom test-flow-watch-mode 'after-save
  "Watch mode for auto-running tests.

Supported:
- 'after-save — trigger on saving project files
- 'file-notify — OS file watcher for project directories (recursive with depth limit)
- nil — disabled"
  :type '(choice (const :tag "after-save" after-save)
                 (const :tag "file-notify" file-notify)
                 (const :tag "disabled" nil))
  :group 'test-flow-watch)

(defcustom test-flow-debounce-seconds 0.7
  "Debounce time (seconds) before running tests after a change."
  :type 'number
  :group 'test-flow-watch)

(defcustom test-flow-watch-include-regexp "\\.el\\'"
  "Regexp for files to include in watch (nil means include all)."
  :type '(choice (const :tag "All files" nil)
                 (regexp :tag "Include regexp"))
  :group 'test-flow-watch)

(defcustom test-flow-watch-exclude-regexp "/\\(?:\\.git\\|\\.direnv\\|node_modules\\|build\\|dist\\|coverage\\)/"
  "Regexp for paths to exclude from watch (nil means exclude none)."
  :type '(choice (const :tag "Exclude none" nil)
                 (regexp :tag "Exclude regexp"))
  :group 'test-flow-watch)

(defcustom test-flow-file-notify-max-depth 3
  "Maximum recursion depth for directory watchers in 'file-notify mode."
  :type 'integer
  :group 'test-flow-watch)

;;;###autoload
(defun test-flow-toggle-watch ()
  "Toggle automatic test running (watch) for the current session (panel-aware)."
  (interactive)
  (declare-function test-flow--find-panel-session "test-flow-panel" ())
  (declare-function test-flow--render "test-flow-render" ())
  (declare-function test-flow-headerline-refresh "test-flow-headerline" ())
  (let* ((sess (or (and (eq major-mode 'test-flow-panel-mode)
                        (ignore-errors (test-flow--find-panel-session)))
                   (test-flow--get-session (test-flow--project-root))))
         (root (test-flow--session-root sess))
         (old (test-flow--get-watch-enabled sess))
         (new (not old)))
    (test-flow--log "toggle-watch: panel=%s root=%s old=%s -> new=%s mode=%s"
                    (buffer-name) root (if old "on" "off") (if new "on" "off")
                    (test-flow--conf sess 'watch-mode (if (boundp 'test-flow-watch-mode) test-flow-watch-mode 'after-save)))
    (test-flow--set-watch-enabled sess new)
    (if new
        (progn
          (test-flow-watch--enable-watch sess)
          (message "test-flow: watch enabled (%s)"
                   (test-flow--conf sess 'watch-mode (if (boundp 'test-flow-watch-mode) test-flow-watch-mode 'after-save))))
      (test-flow-watch--disable-watch sess)
      (message "test-flow: watch disabled"))
    ;; Re-render this session's panel buffer
    (let ((bufname (test-flow--session-panel-name root)))
      (when (and (get-buffer bufname) (fboundp 'test-flow--render))
        (with-current-buffer bufname
          (let ((test-flow--panel-buffer-name bufname))
            (test-flow--render)))))
    ;; Refresh header-line caches and visuals
    (when (fboundp 'test-flow-headerline-refresh)
      (test-flow-headerline-refresh))
    (force-mode-line-update t)
    (test-flow--log "toggle-watch: updated UI for root=%s header+panel refreshed" root)))

;;; Compatibility shims for monolith-era internals
(unless (fboundp 'test-flow--file-event-eligible-p)
  (defalias 'test-flow--file-event-eligible-p 'test-flow-watch--file-event-eligible-p))
(unless (fboundp 'test-flow--collect-dirs)
  (defalias 'test-flow--collect-dirs 'test-flow-watch--collect-dirs))

;; -----------------------------------------------------------------------------
;; Internal helpers (actual implementations used by monolith delegates)
;; -----------------------------------------------------------------------------

(defun test-flow-watch--file-event-eligible-p (file)
  "Return non-nil if FILE should trigger a run according to include/exclude."
  (and (stringp file)
       (or (null test-flow-watch-include-regexp)
           (string-match-p test-flow-watch-include-regexp file))
       (not (and test-flow-watch-exclude-regexp
                 (string-match-p test-flow-watch-exclude-regexp file)))))

(defun test-flow-watch--collect-dirs (root max-depth)
  "Return list of directories under ROOT up to MAX-DEPTH (inclusive ROOT)."
  (let (acc)
    (cl-labels
        ((walk (dir depth)
           (push dir acc)
           (when (> depth 0)
             (dolist (f (directory-files dir t "^[^.]" t))
               (when (file-directory-p f)
                 (let ((f-dir (file-name-as-directory f)))
                   (unless (and test-flow-watch-exclude-regexp
                                (string-match-p test-flow-watch-exclude-regexp f-dir))
                     (walk f (1- depth)))))))))
      (walk root max-depth))
    (cl-remove-duplicates acc :test #'string-equal)))

;; -----------------------------------------------------------------------------
;; Migrated implementations (called by monolith delegates)
;; -----------------------------------------------------------------------------

;; optional (legacy) idle-GC helpers; invoked only if fboundp
;; (declare-function test-flow--ensure-idle-gc-timer "test-flow" ())
;; (declare-function test-flow--cancel-idle-gc-timer-if-unused "test-flow" ())
(defvar test-flow--active-after-save-count 0)

(defun test-flow-watch--schedule-run (sess)
  "Schedule an auto run for SESS with debounce."
  (test-flow--touch-session sess)
  (when (timerp (test-flow--session-debounce-timer sess))
    (cancel-timer (test-flow--session-debounce-timer sess)))
  (let ((delay (test-flow--conf sess 'debounce-seconds (if (boundp 'test-flow-debounce-seconds) test-flow-debounce-seconds 0.7))))
    (setf (test-flow--session-debounce-timer sess)
          (run-at-time
           delay
           nil
           (lambda ()
             (test-flow--touch-session sess)
             (let ((root (test-flow--session-root sess)))
               (test-flow--log "debounce: running for %s" root)
               (with-temp-buffer
                 (cd root)
                 (test-flow-run))))))))

(defun test-flow-watch--after-save-hook ()
  "Trigger tests after saving a relevant file when watch is enabled (session-aware)."
  (let ((file (buffer-file-name)))
    (when file
      (let* ((root (test-flow--project-root))
             (sess (test-flow--get-session root))
             (wmode (test-flow--conf sess 'watch-mode (if (boundp 'test-flow-watch-mode) test-flow-watch-mode 'after-save))))
        (when (and (eq wmode 'after-save)
                   (test-flow--get-watch-enabled sess))
          (let ((test-flow-watch-include-regexp (test-flow--conf sess 'watch-include-regexp (if (boundp 'test-flow-watch-include-regexp) test-flow-watch-include-regexp nil)))
                (test-flow-watch-exclude-regexp (test-flow--conf sess 'watch-exclude-regexp (if (boundp 'test-flow-watch-exclude-regexp) test-flow-watch-exclude-regexp nil))))
            (when (test-flow-watch--file-event-eligible-p file)
              (test-flow--log "after-save: scheduling run for %s" file)
              (test-flow-watch--schedule-run sess))))))))

(defun test-flow-watch--setup-file-notify (sess)
  "Start file-notify watchers for project directories for SESS."
  (let* ((root (test-flow--session-root sess))
         (handles nil)
         (depth (test-flow--conf sess 'file-notify-max-depth (if (boundp 'test-flow-file-notify-max-depth) test-flow-file-notify-max-depth 3)))
         (inc (test-flow--conf sess 'watch-include-regexp (if (boundp 'test-flow-watch-include-regexp) test-flow-watch-include-regexp nil)))
         (exc (test-flow--conf sess 'watch-exclude-regexp (if (boundp 'test-flow-watch-exclude-regexp) test-flow-watch-exclude-regexp nil))))
    (test-flow--touch-session sess)
    (let ((test-flow-watch-exclude-regexp exc))
      (dolist (dir (test-flow-watch--collect-dirs root depth))
        (when (file-directory-p dir)
          (let ((h
                 (file-notify-add-watch
                  dir '(change attribute-change)
                  (lambda (event)
                    (condition-case err
                        (pcase event
                          (`(,_ ,action ,path . ,_)
                           (when (memq action '(created changed deleted renamed moved attribute-changed))
                             (let ((test-flow-watch-include-regexp inc)
                                   (test-flow-watch-exclude-regexp exc))
                               (when (test-flow-watch--file-event-eligible-p (ignore-errors (file-truename path)))
                                 (test-flow--log "file-notify[%s]: %s %s"
                                                 (file-name-nondirectory (directory-file-name root))
                                                 action path)
                                 (test-flow-watch--schedule-run sess))))))
                      (error (test-flow--log "file-notify callback error: %S" err)))))))
            (push h handles)))))
    (setf (test-flow--session-file-notify-handles sess) (nreverse handles))))

(defun test-flow-watch--teardown-file-notify (sess)
  "Stop all active file-notify watchers for SESS."
  (dolist (h (test-flow--session-file-notify-handles sess))
    (ignore-errors (file-notify-rm-watch h)))
  (setf (test-flow--session-file-notify-handles sess) nil))

(defun test-flow-watch--enable-watch (sess)
  "Enable the configured watch mode for SESS."
  (test-flow--touch-session sess)
  (pcase (test-flow--conf sess 'watch-mode (if (boundp 'test-flow-watch-mode) test-flow-watch-mode 'after-save))
    ('after-save
     (unless (member #'test-flow-watch--after-save-hook after-save-hook)
       (add-hook 'after-save-hook #'test-flow-watch--after-save-hook))
     (cl-incf test-flow--active-after-save-count))
    ('file-notify
     (test-flow-watch--setup-file-notify sess))
    (_ nil))
  (when (fboundp 'test-flow--ensure-idle-gc-timer)
    (test-flow--ensure-idle-gc-timer)))

(defun test-flow-watch--disable-watch (sess)
  "Disable watch mode for SESS and cancel any pending timers."
  (pcase (test-flow--conf sess 'watch-mode (if (boundp 'test-flow-watch-mode) test-flow-watch-mode 'after-save))
    ('after-save
     (when (> test-flow--active-after-save-count 0)
       (cl-decf test-flow--active-after-save-count)
       (when (<= test-flow--active-after-save-count 0)
         (remove-hook 'after-save-hook #'test-flow-watch--after-save-hook))))
    ('file-notify
     (test-flow-watch--teardown-file-notify sess)))
  (when (timerp (test-flow--session-debounce-timer sess))
    (cancel-timer (test-flow--session-debounce-timer sess)))
  (setf (test-flow--session-debounce-timer sess) nil)
  (when (fboundp 'test-flow--cancel-idle-gc-timer-if-unused)
    (test-flow--cancel-idle-gc-timer-if-unused)))

(provide 'test-flow-watch)
;;; test-flow-watch.el ends here
