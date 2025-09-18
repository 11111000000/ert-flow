;;; ert-flow.el --- Minimal ERT flow: run external tests, parse, show panel -*- lexical-binding: t; -*-
;; Author: ert-flow bot
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: tools, lisp, testing
;; URL: https://example.com/ert-flow

;;; Commentary:
;; Minimal working step of ert-flow:
;; - Runs an external command (e.g., `emacs -Q --batch -l tests/run-tests.el`)
;; - Parses typical ERT batch output
;; - Renders a right-side panel with grouped results
;; - Copies failures to the kill-ring
;;
;; Configure `ert-flow-external-command' to match your project.
;; Example:
;;   (setq ert-flow-external-command
;;         '("emacs" "-Q" "--batch" "-l" "/home/az/Code/acapella/tests/run-tests.el"))
;;
;; Future steps can add: in-Emacs runner, file-notify watcher, JSON parser, icons, etc.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'project)
(require 'json)
(require 'filenotify)

(defgroup ert-flow nil
  "Automate running and visualizing ERT tests."
  :group 'tools
  :prefix "ert-flow-")

(defcustom ert-flow-external-command nil
  "Command to run tests externally.

May be either:
- a list (PROGRAM ARG1 ARG2 ...)
- or a string to be executed via the user's shell.

Example list value:
  (\"emacs\" \"-Q\" \"--batch\" \"-l\" \"tests/run-tests.el\")"
  :type '(choice (repeat :tag "argv list" string)
                 (string :tag "shell string"))
  :group 'ert-flow)

(defcustom ert-flow-external-failed-args-function nil
  "Function to build extra arguments for running only failed tests.

Called with one argument: a list of failed test names (strings).
Should return a list of strings to append to `ert-flow-external-command'
when building the command. If nil or returns nil, falls back to running all tests.

Note: This requires `ert-flow-external-command' to be a list form (argv).
If it's a shell string, this feature is unavailable."
  :type '(choice (const :tag "Disabled" nil)
                 (function :tag "Function (names -> argv fragment)"))
  :group 'ert-flow)

(defcustom ert-flow-panel-side 'right
  "Panel side for displaying ERT results."
  :type '(choice (const right) (const bottom) (const left) (const top))
  :group 'ert-flow)

(defcustom ert-flow-panel-width 42
  "Panel width in columns for side window display."
  :type 'integer
  :group 'ert-flow)

(defcustom ert-flow-icons t
  "Whether to show unicode status icons in the panel."
  :type 'boolean
  :group 'ert-flow)

(defcustom ert-flow-run-on-enable nil
  "If non-nil, run tests once when `ert-flow-mode' is enabled."
  :type 'boolean
  :group 'ert-flow)

(defcustom ert-flow-log-enabled nil
  "Enable lightweight logging for debugging."
  :type 'boolean
  :group 'ert-flow)

(defcustom ert-flow-watch-mode 'after-save
  "Watch mode for auto-running tests.

Supported:
- 'after-save — trigger on saving project files
- 'file-notify — OS file watcher for project directories (recursive with depth limit)
- nil — disabled"
  :type '(choice (const :tag "after-save" after-save)
                 (const :tag "file-notify" file-notify)
                 (const :tag "disabled" nil))
  :group 'ert-flow)

(defcustom ert-flow-debounce-seconds 0.7
  "Debounce time (seconds) before running tests after a change."
  :type 'number
  :group 'ert-flow)

(defcustom ert-flow-watch-include-regexp "\\.el\\'"
  "Regexp for files to include in watch (nil means include all)."
  :type '(choice (const :tag "All files" nil)
                 (regexp :tag "Include regexp"))
  :group 'ert-flow)

(defcustom ert-flow-watch-exclude-regexp "/\\(?:\\.git\\|\\.direnv\\|node_modules\\|build\\|dist\\)/"
  "Regexp for paths to exclude from watch (nil means exclude none)."
  :type '(choice (const :tag "Exclude none" nil)
                 (regexp :tag "Exclude regexp"))
  :group 'ert-flow)

(defcustom ert-flow-file-notify-max-depth 3
  "Maximum recursion depth for directory watchers in 'file-notify mode."
  :type 'integer
  :group 'ert-flow)

(defcustom ert-flow-parser 'auto
  "Parser mode for interpreting test output.

- 'auto: try JSON first, then fallback to batch ERT parsing
- 'json: expect JSON object with fields 'summary' and 'tests'
- 'ert-batch: parse standard ERT batch output"
  :type '(choice (const auto) (const json) (const ert-batch))
  :group 'ert-flow)

(defconst ert-flow--panel-buffer-name "*ert-flow*"
  "Name of the ert-flow panel buffer.")

(defconst ert-flow--details-buffer-name "*ert-flow: details*"
  "Name of the ert-flow details buffer.")

(defconst ert-flow--status-icons
  '((pass  . "✓")
    (fail  . "✕")
    (error . "!")
    (skip  . "∼")
    (xfail . "∼"))
  "Mapping of status symbol to a short visual glyph.")

(defvar ert-flow--last-raw-output nil
  "Raw stdout of the last external test run.")

(defvar ert-flow--last-results nil
  "List of test result alists from the last run.")

(defvar ert-flow--last-summary nil
  "Alist of summary fields from the last run.")

(defvar ert-flow--process nil
  "Current running process object, if any.")

(defvar ert-flow--debounce-timer nil
  "Internal timer used to debounce auto-runs.")

(defvar ert-flow--watch-enabled nil
  "Non-nil when watch is enabled.")

(defvar ert-flow--file-notify-handles nil
  "List of file-notify descriptors for active watchers.")

(defun ert-flow--log (fmt &rest args)
  "Log a debug message FMT with ARGS when `ert-flow-log-enabled' is non-nil."
  (when ert-flow-log-enabled
    (apply #'message (concat "[ert-flow] " fmt) args)))

;;;; Utilities

(defun ert-flow--string-trim (s)
  "Trim whitespace around string S."
  (when (stringp s)
    (string-trim s)))

(defun ert-flow--suite-of (test-name)
  "Return suite name derived from TEST-NAME (prefix until first '/')."
  (let* ((s (or test-name ""))
         (idx (string-match "/" s)))
    (if (and idx (> idx 0))
        (substring s 0 idx)
      s)))

(defun ert-flow--status-icon (status)
  "Return icon string for STATUS."
  (or (cdr (assq status ert-flow--status-icons)) "?"))

(defun ert-flow--normalize-command (value)
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

;;;; Parsing ERT batch output

(defun ert-flow--parse-batch-output (out)
  "Parse ERT batch OUT and return (SUMMARY . RESULTS).
SUMMARY is an alist like:
  ((total . N) (unexpected . M) (time . STRING-OR-NIL))
RESULTS is a list of alists:
  (:name STRING :status SYMBOL :message STRING :details STRING :suite STRING)

Recognized statuses: pass, fail, error, skip, xfail."
  (let* ((lines (split-string (or out "") "\n"))
         (name->status (make-hash-table :test 'equal))
         (name->details (make-hash-table :test 'equal))
         (all-names (make-hash-table :test 'equal))
         (total nil)
         (unexpected nil)
         (time-str nil))
    ;; Pass 1: gather per-test details blocks and pass markers.
    (cl-loop
     with i = 0
     while (< i (length lines))
     for line = (nth i lines)
     do
     ;; Test <name> passed.
     (when (string-match "^Test[ \t]+\\([^ \t]+\\)[ \t]+passed\\.$" line)
       (let ((nm (match-string 1 line)))
         (puthash nm t all-names)
         (puthash nm 'pass name->status)))
     ;; Test <name> backtrace:/condition: blocks
     (when (string-match "^Test[ \t]+\\([^ \t]+\\)[ \t]+\\(backtrace\\|condition\\):$" line)
       (let* ((nm (match-string 1 line))
              (start (1+ i))
              (j start))
         (while (and (< j (length lines))
                     (not (string-match "^Test[ \t]+" (nth j lines))))
           (cl-incf j))
         (let ((block (string-join (cl-subseq lines start j) "\n")))
           (puthash nm t all-names)
           (puthash nm (ert-flow--string-trim block) name->details))
         (setq i (1- j))))
     ;; Summary captures
     (when (and (null total)
                (string-match "^Ran[ \t]+\\([0-9]+\\)[ \t]+tests?" line))
       (setq total (string-to-number (match-string 1 line))))
     (when (and (null unexpected)
                (string-match "^\\([0-9]+\\)[ \t]+unexpected results?:" line))
       (setq unexpected (string-to-number (match-string 1 line))))
     (when (and (null time-str)
                (string-match "in[ \t]+\\([0-9.]+\\)[ \t]+seconds" line))
       (setq time-str (match-string 1 line)))
     (cl-incf i))
    ;; Pass 2: statuses from the final section (FAILED/ERROR/SKIPPED NAME)
    (dolist (line lines)
      (when (string-match "^[ \t]*\\(FAILED\\|ERROR\\|SKIPPED\\)[ \t]+\\(.+\\)$" line)
        (let* ((kw (match-string 1 line))
               (nm (ert-flow--string-trim (match-string 2 line)))
               (st (pcase kw
                     ("FAILED" 'fail)
                     ("ERROR"  'error)
                     ("SKIPPED" 'skip)
                     (_ 'fail))))
          (puthash nm t all-names)
          (puthash nm st name->status))))
    ;; Build results list
    (let (results)
      (maphash
       (lambda (nm _)
         (let* ((status (or (gethash nm name->status) 'fail))
                (details (or (gethash nm name->details) ""))
                (message (car (split-string details "\n" t)))
                (suite (ert-flow--suite-of nm)))
           (push (list :name nm
                       :status status
                       :message (or message (symbol-name status))
                       :details details
                       :suite suite)
                 results)))
       all-names)
      (unless total
        (setq total (length results)))
      (unless unexpected
        (setq unexpected (cl-count-if
                          (lambda (r)
                            (memq (plist-get r :status) '(fail error)))
                          results)))
      (let ((summary `((total . ,total)
                       (unexpected . ,unexpected)
                       (time . ,time-str))))
        (cons summary (nreverse results))))))

;;;; JSON parsing and auto-dispatch

(defun ert-flow--json-safe-substring (s)
  "Return substring of S from first { to last }, or nil."
  (when (stringp s)
    (let* ((start (string-match "{" s))
           (end   (and start (save-match-data (string-match ".*}" s)))))
      (when (and start end)
        (substring s start (1+ end))))))

(defun ert-flow--parse-json-output (out)
  "Parse JSON OUT and return (SUMMARY . RESULTS) or nil on failure.

Expected top-level alist keys:
- tests: list of alists with 'name', 'status', optional 'message', 'details', 'file', 'line', 'tags'
- summary: alist with 'total', 'passed', 'failed', 'error', 'skipped', 'duration_ms' (optional)"
  (condition-case _err
      (let* ((json-str (or (ert-flow--json-safe-substring out) out))
             (obj (json-parse-string json-str :object-type 'alist :array-type 'list))
             (tests (alist-get 'tests obj))
             (summary (alist-get 'summary obj)))
        (unless (listp tests)
          (signal 'wrong-type-argument (list 'list tests)))
        (let* ((results
                (mapcar
                 (lambda (tobj)
                   (let* ((nm (alist-get 'name tobj))
                          (st-str (alist-get 'status tobj))
                          (st (pcase (and st-str (downcase st-str))
                                ("pass" 'pass) ("ok" 'pass)
                                ("fail" 'fail) ("failed" 'fail)
                                ("error" 'error)
                                ("skip" 'skip) ("skipped" 'skip)
                                ("xfail" 'xfail)
                                (_ 'fail)))
                          (msg (alist-get 'message tobj))
                          (det (alist-get 'details tobj))
                          (suite (ert-flow--suite-of nm)))
                     (list :name nm :status st
                           :message (or msg (and det (car (split-string det "\n" t))) (symbol-name st))
                           :details (or det "")
                           :suite suite)))
                 tests))
               (total (or (alist-get 'total summary) (length results)))
               (unexpected (seq-count
                            (lambda (r) (memq (plist-get r :status) '(fail error)))
                            results))
               (time (or (alist-get 'duration_ms summary)
                         (alist-get 'time summary))))
          (cons `((total . ,total) (unexpected . ,unexpected) (time . ,(and time (format "%s" time))))
                results)))
    (error nil)))

(defun ert-flow--parse-output (out)
  "Dispatch parsing according to `ert-flow-parser'."
  (pcase ert-flow-parser
    ('json (or (ert-flow--parse-json-output out)
               (cons '((total . 0) (unexpected . 0) (time . nil)) '())))
    ('ert-batch (ert-flow--parse-batch-output out))
    ('auto (or (ert-flow--parse-json-output out)
               (ert-flow--parse-batch-output out)))
    (_ (ert-flow--parse-batch-output out))))

;;;; Rendering

(defvar ert-flow-panel-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'ert-flow-run)
    (define-key map (kbd "r") #'ert-flow-run)
    (define-key map (kbd "f") #'ert-flow-run-failed)
    (define-key map (kbd "w") #'ert-flow-toggle-watch)
    (define-key map (kbd "d") #'ert-flow-detect-runner)
    (define-key map (kbd "c") #'ert-flow-copy-failures)
    (define-key map (kbd "x") #'ert-flow-clear)
    (define-key map (kbd "o") #'ert-flow-goto-definition-at-point)
    (define-key map (kbd "RET") #'ert-flow-open-details-at-point)
    map)
  "Keymap for `ert-flow-panel-mode'.")

(define-derived-mode ert-flow-panel-mode special-mode "ert-flow-panel"
  "Major mode for displaying ERT results in a side panel."
  (setq buffer-read-only t
        truncate-lines t))

;;;###autoload
(defun ert-flow-open-panel ()
  "Open or focus the ert-flow panel."
  (interactive)
  (let* ((buf (get-buffer-create ert-flow--panel-buffer-name))
         (win (display-buffer-in-side-window
               buf
               `((side . ,ert-flow-panel-side)
                 (window-width . ,ert-flow-panel-width)))))
    (with-current-buffer buf
      (ert-flow-panel-mode)
      (ert-flow--render))
    (select-window win)))

(defun ert-flow--propertize (text result)
  "Return TEXT with RESULT stored as a text property."
  (propertize text 'ert-flow--result result))

(defun ert-flow--render ()
  "Render the panel using `ert-flow--last-summary' and `ert-flow--last-results'."
  (let ((buf (get-buffer-create ert-flow--panel-buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (let ((watch (if ert-flow--watch-enabled "On" "Off")))
          (insert (propertize (format "ert-flow  [Watch:%s]  " watch)
                              'face 'mode-line-buffer-id)))
        (insert (propertize "[r:Run] [f:Run failed] [w:Toggle Watch] [d:Detect] [c:Copy failures] [x:Clear] [o:Goto] [RET:Details]\n"
                            'face 'shadow))
        (let* ((sum ert-flow--last-summary)
               (total (alist-get 'total sum))
               (unexpected (alist-get 'unexpected sum))
               (time (alist-get 'time sum)))
          (insert (format "Summary: %s tests, %s unexpected, time: %s\n\n"
                          (or total 0) (or unexpected 0) (or time "-"))))
        ;; Group by suite
        (let* ((groups (make-hash-table :test 'equal)))
          (dolist (r ert-flow--last-results)
            (let* ((suite (or (plist-get r :suite) ""))
                   (bucket (gethash suite groups)))
              (puthash suite (cons r bucket) groups)))
          (let (suites)
            (maphash (lambda (k _v) (push k suites)) groups)
            (dolist (suite (sort suites #'string<))
              (let ((rs (reverse (gethash suite groups))))
                (insert (propertize (format "%s\n" suite)
                                    'face 'bold))
                (dolist (r rs)
                  (let* ((st (plist-get r :status))
                         (nm (plist-get r :name))
                         (icon (if ert-flow-icons (ert-flow--status-icon st) ""))
                         (line (format "  %s %s\n" icon nm)))
                    (insert (ert-flow--propertize line r)))))))))
      (goto-char (point-min)))))

(defun ert-flow-open-details-at-point ()
  "Open a buffer with details for the test at point."
  (interactive)
  (let* ((r (get-text-property (line-beginning-position) 'ert-flow--result)))
    (if (null r)
        (user-error "No test result at point")
      (let ((buf (get-buffer-create ert-flow--details-buffer-name)))
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (read-only-mode -1)
            (erase-buffer)
            (insert (format "%s\n\n" (plist-get r :name)))
            (insert (or (plist-get r :details) "No details"))
            (goto-char (point-min))
            (view-mode 1)))
        (display-buffer buf)))))

;;; Navigation

;;;###autoload
(defun ert-flow-goto-definition-at-point ()
  "Jump to the ERT test definition at point.

Tries to intern the test name and call `find-function'.
Works when the test function is loaded in the current Emacs session."
  (interactive)
  (let* ((r (get-text-property (line-beginning-position) 'ert-flow--result))
         (name (and r (plist-get r :name))))
    (unless name
      (user-error "No test result at point"))
    (let ((sym (intern-soft name)))
      (unless (and sym (fboundp sym))
        (user-error "Test function %s is not loaded; load tests in this Emacs to jump" name))
      (find-function sym))))

;;;; Running external command

;;;###autoload
(defun ert-flow-run-failed ()
  "Run only failed/error tests if possible, else run all.

Uses `ert-flow-external-failed-args-function' to build extra argv for
`ert-flow-external-command' (must be a list form). If unavailable or
invalid, falls back to `ert-flow-run'."
  (interactive)
  (let* ((fails
          (mapcar (lambda (r) (plist-get r :name))
                  (seq-filter (lambda (r) (memq (plist-get r :status) '(fail error)))
                              (or ert-flow--last-results '())))))
    (cond
     ((null fails)
      (message "ert-flow: no failed tests to run")
      (ert-flow-run))
     ((or (null ert-flow-external-failed-args-function)
          (not (listp ert-flow-external-command)))
      (message "ert-flow: cannot run failed selectively; running all")
      (ert-flow-run))
     (t
      (let* ((base ert-flow-external-command)
             (extra (ignore-errors (funcall ert-flow-external-failed-args-function fails))))
        (if (not (and (listp extra) (seq-every-p #'stringp extra)))
            (progn
              (message "ert-flow: bad failed-args; running all")
              (ert-flow-run))
          (let ((cmd (append base extra)))
            (when (process-live-p ert-flow--process)
              (ert-flow--log "Killing previous process...")
              (ignore-errors (kill-process ert-flow--process)))
            (setq ert-flow--last-raw-output nil)
            (ert-flow-open-panel)
            (with-current-buffer (get-buffer-create ert-flow--panel-buffer-name)
              (let ((inhibit-read-only t))
                (goto-char (point-max))
                (insert (propertize (format "Running (failed): %S\n" cmd) 'face 'shadow))))
            (setq ert-flow--process
                  (let ((default-directory (ert-flow--project-root)))
                    (make-process
                     :name "ert-flow-runner"
                     :command cmd
                     :noquery t
                     :buffer nil
                     :connection-type 'pipe
                     :filter #'ert-flow--proc-filter
                     :sentinel #'ert-flow--proc-sentinel))))))))))

;;;###autoload
(defun ert-flow-run ()
  "Run the configured external command and update the panel.

Customize `ert-flow-external-command' to point to your test runner."
  (interactive)
  (let ((cmd (ert-flow--normalize-command ert-flow-external-command)))
    (unless cmd
      (user-error "Set `ert-flow-external-command' to a list or string command"))
    (when (process-live-p ert-flow--process)
      (ert-flow--log "Killing previous process...")
      (ignore-errors (kill-process ert-flow--process)))
    (setq ert-flow--last-raw-output nil)
    (ert-flow-open-panel)
    (with-current-buffer (get-buffer-create ert-flow--panel-buffer-name)
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert (propertize (format "Running: %S\n" cmd) 'face 'shadow))))
    (setq ert-flow--process
          (let ((default-directory (ert-flow--project-root)))
            (make-process
             :name "ert-flow-runner"
             :command cmd
             :noquery t
             :buffer nil
             :connection-type 'pipe
             :filter #'ert-flow--proc-filter
             :sentinel #'ert-flow--proc-sentinel)))))

(defun ert-flow--proc-filter (_proc chunk)
  "Accumulate CHUNK from the running process."
  (condition-case err
      (progn
        (setq ert-flow--last-raw-output
              (concat (or ert-flow--last-raw-output "") chunk)))
    (error
     (ert-flow--log "Filter error: %S" err))))

(defun ert-flow--proc-sentinel (_proc event)
  "Handle process EVENT."
  (condition-case err
      (progn
        (ert-flow--log "Sentinel: %s" (string-trim (or event "")))
        (when (and (stringp event)
                   (string-match-p "\\(finished\\|exited\\)" event))
          (let* ((parsed (ert-flow--parse-output ert-flow--last-raw-output))
                 (summary (car parsed))
                 (results (cdr parsed)))
            (setq ert-flow--last-summary summary
                  ert-flow--last-results results)
            (ert-flow--render))))
    (error
     (ert-flow--log "Sentinel error: %S" err))))

;;;; Runner helpers

;;;###autoload
(defun ert-flow-detect-runner ()
  "Detect tests/run-tests.el under project root and set `ert-flow-external-command'.
Looks for tests/run-tests.el or test/run-tests.el."
  (interactive)
  (let* ((root (ert-flow--project-root))
         (candidates '("tests/run-tests.el" "test/run-tests.el"))
         (found (seq-find (lambda (rel)
                            (file-exists-p (expand-file-name rel root)))
                          candidates)))
    (if (not found)
        (user-error "Could not find tests/run-tests.el under %s" root)
      (setq ert-flow-external-command
            (list "emacs" "-Q" "--batch" "-l" (expand-file-name found root)))
      (message "ert-flow: external command set to %S" ert-flow-external-command))))

;;;; Watcher

(defun ert-flow--project-root ()
  "Return current project root directory as a string, or `default-directory'."
  (let ((proj (project-current nil default-directory)))
    (if proj
        (expand-file-name (project-root proj))
      (expand-file-name default-directory))))

(defun ert-flow--buffer-in-project-p (buf)
  "Return non-nil if BUF's file is under the current project root."
  (when-let* ((file (buffer-local-value 'buffer-file-name buf)))
    (let* ((root (file-name-as-directory (ert-flow--project-root)))
           (abs  (expand-file-name file)))
      (string-prefix-p root abs))))

(defun ert-flow--schedule-run ()
  "Schedule an auto run with debounce."
  (when (timerp ert-flow--debounce-timer)
    (cancel-timer ert-flow--debounce-timer))
  (setq ert-flow--debounce-timer
        (run-at-time ert-flow-debounce-seconds nil #'ert-flow-run)))

(defun ert-flow--after-save-hook ()
  "Trigger tests after saving a relevant file when watch is enabled."
  (when (and ert-flow--watch-enabled
             (eq ert-flow-watch-mode 'after-save)
             (ert-flow--buffer-in-project-p (current-buffer))
             (string-equal (file-name-extension (or (buffer-file-name) "")) "el"))
    (ert-flow--log "after-save: scheduling run for %s" (buffer-file-name))
    (ert-flow--schedule-run)))

(defun ert-flow--file-event-eligible-p (file)
  "Return non-nil if FILE should trigger a run according to include/exclude."
  (and (stringp file)
       (or (null ert-flow-watch-include-regexp)
           (string-match-p ert-flow-watch-include-regexp file))
       (not (and ert-flow-watch-exclude-regexp
                 (string-match-p ert-flow-watch-exclude-regexp file)))))

(defun ert-flow--collect-dirs (root max-depth)
  "Return list of directories under ROOT up to MAX-DEPTH (inclusive ROOT)."
  (let (acc)
    (cl-labels
        ((walk (dir depth)
           (push dir acc)
           (when (> depth 0)
             (dolist (f (directory-files dir t "^[^.]" t))
               (when (file-directory-p f)
                 (unless (and ert-flow-watch-exclude-regexp
                              (string-match-p ert-flow-watch-exclude-regexp f))
                   (walk f (1- depth))))))))
      (walk root max-depth))
    (cl-remove-duplicates acc :test #'string-equal)))

(defun ert-flow--file-notify-callback (event)
  "Handle file-notify EVENT: schedule a run for interesting changes."
  (condition-case err
      (pcase event
        (`(,_ ,action ,file . ,_)
         (when (memq action '(created changed deleted renamed moved attribute-changed))
           (when (ert-flow--file-event-eligible-p (ignore-errors (file-truename file)))
             (ert-flow--log "file-notify: %s %s" action file)
             (ert-flow--schedule-run)))))
    (error (ert-flow--log "file-notify callback error: %S" err))))

(defun ert-flow--setup-file-notify ()
  "Start file-notify watchers for project directories."
  (let ((root (ert-flow--project-root)))
    (dolist (dir (ert-flow--collect-dirs root ert-flow-file-notify-max-depth))
      (when (file-directory-p dir)
        (push (file-notify-add-watch dir '(change attribute-change) #'ert-flow--file-notify-callback)
              ert-flow--file-notify-handles)))))

(defun ert-flow--teardown-file-notify ()
  "Stop all active file-notify watchers."
  (dolist (h ert-flow--file-notify-handles)
    (ignore-errors (file-notify-rm-watch h)))
  (setq ert-flow--file-notify-handles nil))

(defun ert-flow--enable-watch ()
  "Enable the configured watch mode."
  (pcase ert-flow-watch-mode
    ('after-save
     (add-hook 'after-save-hook #'ert-flow--after-save-hook))
    ('file-notify
     (ert-flow--setup-file-notify))
    (_ nil)))

(defun ert-flow--disable-watch ()
  "Disable watch mode and cancel any pending timers."
  (remove-hook 'after-save-hook #'ert-flow--after-save-hook)
  (ert-flow--teardown-file-notify)
  (when (timerp ert-flow--debounce-timer)
    (cancel-timer ert-flow--debounce-timer))
  (setq ert-flow--debounce-timer nil))

;;;###autoload
(defun ert-flow-toggle-watch ()
  "Toggle automatic test running (watch)."
  (interactive)
  (setq ert-flow--watch-enabled (not ert-flow--watch-enabled))
  (if ert-flow--watch-enabled
      (progn
        (ert-flow--enable-watch)
        (message "ert-flow: watch enabled (%s)" ert-flow-watch-mode))
    (ert-flow--disable-watch)
    (message "ert-flow: watch disabled"))
  (when (get-buffer ert-flow--panel-buffer-name)
    (ert-flow--render)))

;;;; Copy failures

;;;###autoload
(defun ert-flow-copy-failures ()
  "Copy failures/errors from the last run into the kill-ring."
  (interactive)
  (let* ((fails (seq-filter
                 (lambda (r) (memq (plist-get r :status) '(fail error)))
                 (or ert-flow--last-results '())))
         (header (format "ERT failures (%s): %d\n"
                         (format-time-string "%Y-%m-%d %H:%M:%S")
                         (length fails))))
    (if (null fails)
        (progn
          (kill-new (concat header "No failures."))
          (message "ert-flow: no failures"))
      (let ((body
             (mapconcat
              (lambda (r)
                (let* ((nm (plist-get r :name))
                       (msg (or (plist-get r :message) "")))
                  (format "- %s: %s" nm msg)))
              fails
              "\n"))
            (details
             (mapconcat
              (lambda (r)
                (let ((nm (plist-get r :name))
                      (det (or (plist-get r :details) "")))
                  (format "\n=== %s ===\n%s" nm det)))
              fails
              "")))
        (kill-new (concat header body "\n" details))
        (message "ert-flow: failures copied")))))

;;;###autoload
(defun ert-flow-clear ()
  "Clear panel and last results."
  (interactive)
  (setq ert-flow--last-summary nil
        ert-flow--last-results nil
        ert-flow--last-raw-output nil)
  (when (get-buffer ert-flow--panel-buffer-name)
    (ert-flow--render))
  (message "ert-flow: cleared"))

;;;; Minor mode

;;;###autoload
(define-minor-mode ert-flow-mode
  "Toggle ert-flow mode.

When enabled, opens the panel. If `ert-flow-run-on-enable' is non-nil,
runs tests immediately using `ert-flow-run'."
  :global t
  :group 'ert-flow
  (if ert-flow-mode
      (progn
        (ert-flow-open-panel)
        (when ert-flow-run-on-enable
          (ert-flow-run)))
    ;; on disable: close panel window but keep buffers
    (let ((buf (get-buffer ert-flow--panel-buffer-name)))
      (when buf
        (dolist (w (get-buffer-window-list buf nil t))
          (when (window-parameter w 'window-side)
            (delete-window w)))))))

(provide 'ert-flow)
;;; ert-flow.el ends here
