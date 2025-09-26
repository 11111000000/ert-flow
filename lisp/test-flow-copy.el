;;; test-flow-copy.el --- Copy/share helpers for test-flow  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Provides actual copy-failures implementation using core API, independent
;; from the monolith. test-flow.el delegates here.
;;
;; Exposed entry:
;; - test-flow-copy--impl-copy-failures (interactive, called by test-flow.el)
;;
;; Honors user options when available:
;; - test-flow-copy-format ('plain | 'org | 'markdown)       [default: 'plain]
;; - test-flow-copy-backtrace-limit (integer | nil)          [default: nil]
;; - test-flow-copy-include-stdout (boolean)                 [default: nil]
;; - test-flow-copy-include-stderr (boolean)                 [default: nil]

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'test-flow-core)

;;;###autoload
(defgroup test-flow-copy nil
  "Copy/share utilities for test-flow."
  :group 'test-flow)

;; Options (define here so tests can dynamically `let' bind them)
(defcustom test-flow-copy-format 'plain
  "Format for copying failures: 'plain (default), 'org, or 'markdown."
  :type '(choice (const plain) (const org) (const markdown))
  :group 'test-flow-copy)

(defcustom test-flow-copy-backtrace-limit nil
  "If non-nil, truncate each details/backtrace to this many characters."
  :type '(choice (const :tag "No limit" nil) integer)
  :group 'test-flow-copy)

(defcustom test-flow-copy-include-stdout nil
  "If non-nil, include the session's raw stdout tail in the copied failures block."
  :type 'boolean
  :group 'test-flow-copy)

(defcustom test-flow-copy-include-stderr nil
  "If non-nil, include the session's raw stderr tail in the copied failures block."
  :type 'boolean
  :group 'test-flow-copy)

;; Safe readers for variables that might be defined in monolith
(defmacro test-flow-copy--opt (sym default)
  `(if (boundp ',sym) ,sym ,default))

(defun test-flow-copy--trim (s limit)
  "Trim string S to LIMIT characters, appending a truncation marker."
  (if (and (integerp limit) (stringp s) (> (length s) limit))
      (concat (substring s 0 limit) "\n... [truncated]")
    (or s "")))

(defun test-flow-copy--format-header (timestamp n proj runner cmd duration)
  "Return header string for TIMESTAMP, N, PROJ, RUNNER, CMD, DURATION."
  (let ((meta (concat (format "Project: %s | Runner: %s" proj runner)
                      (if (and (stringp cmd) (> (length cmd) 0))
                          (format " | Command: %s" cmd) "")
                      (if (and (stringp duration) (> (length duration) 0))
                          (format " | Duration: %s" duration) ""))))
    (pcase (test-flow-copy--opt test-flow-copy-format 'plain)
      ('plain    (format "ERT failures (%s): %d\n%s\n" timestamp n meta))
      ('org      (format "* ERT failures (%s): %d\n%s\n" timestamp n meta))
      ('markdown (format "### ERT failures (%s): %d\n%s\n" timestamp n meta))
      (_         (format "ERT failures (%s): %d\n%s\n" timestamp n meta)))))

(defun test-flow-copy--format-item (r)
  "Return a single list item line for result plist R."
  (let* ((nm (plist-get r :name))
         (msg (or (plist-get r :message) ""))
         (file (plist-get r :file))
         (line (plist-get r :line))
         (loc (cond
               ((and file line) (format " (%s:%s)" file line))
               (file (format " (%s)" file))
               (t ""))))
    (pcase (test-flow-copy--opt test-flow-copy-format 'plain)
      ('plain    (format "- %s%s: %s" nm loc msg))
      ('org      (format "- %s%s :: %s" nm loc msg))
      ('markdown (format "- %s%s: %s" nm loc msg))
      (_         (format "- %s%s: %s" nm loc msg)))))

(defun test-flow-copy--format-details (name details)
  "Return details block for NAME and DETAILS text."
  (let* ((limit (test-flow-copy--opt test-flow-copy-backtrace-limit nil))
         (txt (test-flow-copy--trim (or details "") limit)))
    (pcase (test-flow-copy--opt test-flow-copy-format 'plain)
      ('plain    (format "\n=== %s ===\n%s" name txt))
      ('org      (format "\n** %s\n#+begin_example\n%s\n#+end_example\n" name txt))
      ('markdown (format "\n#### %s\n=\n%s\n=\n" name txt))
      (_         (format "\n=== %s ===\n%s" name txt)))))

(defun test-flow-copy--stdout-tail (raw)
  "Return formatted stdout tail block from RAW or nil."
  (when (and (test-flow-copy--opt test-flow-copy-include-stdout nil)
             (stringp raw) (> (length raw) 0))
    (let* ((limit (test-flow-copy--opt test-flow-copy-backtrace-limit nil))
           (tail (if (and (integerp limit) (> (length raw) limit))
                     (substring raw (- (length raw) limit))
                   raw)))
      (pcase (test-flow-copy--opt test-flow-copy-format 'plain)
        ('plain    (format "\n--- STDOUT tail ---\n%s\n" tail))
        ('org      (format "\n** STDOUT tail\n#+begin_example\n%s\n#+end_example\n" tail))
        ('markdown (format "\n#### STDOUT tail\n=\n%s\n=\n" tail))
        (_         (format "\n--- STDOUT tail ---\n%s\n" tail))))))

(defun test-flow-copy--stderr-tail (raw)
  "Return formatted stderr tail block from RAW or nil."
  (when (and (test-flow-copy--opt test-flow-copy-include-stderr nil)
             (stringp raw) (> (length raw) 0))
    (let* ((limit (test-flow-copy--opt test-flow-copy-backtrace-limit nil))
           (tail (if (and (integerp limit) (> (length raw) limit))
                     (substring raw (- (length raw) limit))
                   raw)))
      (pcase (test-flow-copy--opt test-flow-copy-format 'plain)
        ('plain    (format "\n--- STDERR tail ---\n%s\n" tail))
        ('org      (format "\n** STDERR tail\n#+begin_example\n%s\n#+end_example\n" tail))
        ('markdown (format "\n#### STDERR tail\n=\n%s\n=\n" tail))
        (_         (format "\n--- STDERR tail ---\n%s\n" tail))))))

(defun test-flow-copy--gather-context ()
  "Collect context for building the copy string. Return plist."
  (let* ((root (test-flow--project-root))
         (sess (test-flow--get-session root))
         ;; Prefer session-scoped values; fall back to legacy globals for compatibility with tests.
         (results (or (and sess (test-flow--get-last-results sess))
                      (and (boundp 'test-flow--last-results) test-flow--last-results)))
         (sum     (or (and sess (test-flow--get-last-summary sess))
                      (and (boundp 'test-flow--last-summary) test-flow--last-summary)))
         (raw     (or (and sess (test-flow--get-last-raw-output sess))
                      (and (boundp 'test-flow--last-raw-output) test-flow--last-raw-output)))
         (fails (seq-filter (lambda (r) (memq (plist-get r :status) '(fail error)))
                            (or results '())))
         (ts (format-time-string "%Y-%m-%d %H:%M:%S"))
         (proj (file-name-nondirectory (directory-file-name root)))
         ;; Runner info (safe fallbacks)
         (runner-sym (test-flow--conf sess 'runner (if (boundp 'test-flow-runner) test-flow-runner 'external-command)))
         (runner (if (eq runner-sym 'in-emacs-ert) "in-emacs" "external"))
         (ext (test-flow--conf sess 'external-command (if (boundp 'test-flow-external-command) test-flow-external-command nil)))
         (cmd-str (cond
                   ((and (eq runner-sym 'external-command) (listp ext)) (mapconcat #'identity ext " "))
                   ((and (eq runner-sym 'external-command) (stringp ext)) ext)
                   (t "")))
         (dur-ms (and (listp sum) (alist-get 'duration-ms sum)))
         (dur-str (cond ((numberp dur-ms) (format "%.3fs" (/ dur-ms 1000.0))) (t ""))))
    (list :root root :sess sess :results results :sum sum :raw raw
          :fails fails :ts ts :proj proj :runner runner :cmd-str cmd-str :dur-str dur-str)))

(defun test-flow-copy--build-string (ctx)
  "Build final copy string from context CTX."
  (let* ((fails (plist-get ctx :fails))
         (ts (plist-get ctx :ts))
         (proj (plist-get ctx :proj))
         (runner (plist-get ctx :runner))
         (cmd-str (plist-get ctx :cmd-str))
         (dur-str (plist-get ctx :dur-str))
         (raw (plist-get ctx :raw))
         (sess (plist-get ctx :sess)))
    (if (null fails)
        (format "ERT failures (%s): 0\nProject: %s | Runner: %s%s%s\nNo failures."
                ts proj runner
                (if (> (length cmd-str) 0) (format " | Command: %s" cmd-str) "")
                (if (> (length dur-str) 0) (format " | Duration: %s" dur-str) ""))
      (let* ((header (test-flow-copy--format-header ts (length fails) proj runner cmd-str dur-str))
             (body (mapconcat #'test-flow-copy--format-item fails "\n"))
             (details (mapconcat
                       (lambda (r)
                         (test-flow-copy--format-details (plist-get r :name)
                                                         (plist-get r :details)))
                       fails ""))
             (stdout-block (test-flow-copy--stdout-tail raw))
             (stderr-raw (or (and sess (test-flow--get-last-stderr-output sess))
                             (and (boundp 'test-flow--last-stderr-output) test-flow--last-stderr-output)))
             (stderr-block (test-flow-copy--stderr-tail stderr-raw)))
        (concat header body "\n" details (or stdout-block "") (or stderr-block ""))))))

;;;###autoload
(defun test-flow-copy--impl-copy-failures ()
  "Copy failures/errors from the last run into the kill-ring (with backtraces)."
  (interactive)
  (let* ((ctx (test-flow-copy--gather-context))
         (fails (plist-get ctx :fails))
         (s (test-flow-copy--build-string ctx)))
    (kill-new s)
    (message (if fails "test-flow: failures copied" "test-flow: no failures"))))

(provide 'test-flow-copy)
;;; test-flow-copy.el ends here
