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
(require 'ert)
(require 'seq)
(require 'project)
(require 'json)
(require 'filenotify)
(require 'all-the-icons nil t)

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
  "Whether to show unicode status icons in the panel (per-test status icons)."
  :type 'boolean
  :group 'ert-flow)

(defcustom ert-flow-toolbar-style 'auto
  "Toolbar rendering style.

- 'auto  — use all-the-icons if available, otherwise use Unicode/text
- 'icons — require all-the-icons (fallback to text if unavailable)
- 'text  — always use text (no icon glyphs)"
  :type '(choice (const auto) (const icons) (const text))
  :group 'ert-flow)

(defface ert-flow-face-pass
  '((t :inherit success))
  "Face for passed test icons."
  :group 'ert-flow)

(defface ert-flow-face-fail
  '((t :inherit error))
  "Face for failed test icons."
  :group 'ert-flow)

(defface ert-flow-face-error
  '((t :foreground "orange red" :weight bold))
  "Face for error test icons."
  :group 'ert-flow)

(defface ert-flow-face-skip
  '((t :inherit shadow))
  "Face for skipped/xfail test icons."
  :group 'ert-flow)

(defface ert-flow-toolbar-run
  '((t :foreground "SpringGreen3" :weight bold))
  "Face for the Run toolbar button."
  :group 'ert-flow)

(defface ert-flow-toolbar-run-failed
  '((t :foreground "DarkOrange2" :weight bold))
  "Face for the Run failed toolbar button."
  :group 'ert-flow)

(defface ert-flow-toolbar-watch
  '((t :foreground "DeepSkyBlue3" :weight bold))
  "Face for the Watch toolbar button."
  :group 'ert-flow)

(defface ert-flow-toolbar-copy
  '((t :foreground "SteelBlue3" :weight bold))
  "Face for the Copy toolbar button."
  :group 'ert-flow)

(defface ert-flow-toolbar-clear
  '((t :foreground "gray60"))
  "Face for the Clear toolbar button."
  :group 'ert-flow)

(defface ert-flow-toolbar-detect
  '((t :foreground "MediumPurple3" :weight bold))
  "Face for the Detect toolbar button."
  :group 'ert-flow)

(defface ert-flow-toolbar-goto
  '((t :foreground "Gold3" :weight bold))
  "Face for the Goto toolbar button."
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

(defcustom ert-flow-runner 'external-command
  "Runner backend to use.

- 'external-command — run an external process (default)
- 'in-emacs-ert   — run tests inside current Emacs via ERT and parse batch output

Note: 'in-emacs-ert runs synchronously and may block UI for long suites, but
it avoids external processes and provides immediate access to loaded tests."
  :type '(choice (const external-command) (const in-emacs-ert))
  :group 'ert-flow)

(defcustom ert-flow-max-concurrent-runs 3
  "Maximum number of concurrent test processes across all sessions."
  :type 'integer
  :group 'ert-flow)

(defcustom ert-flow-session-idle-seconds 120
  "Seconds of inactivity after which a session's watcher is auto-disabled."
  :type 'integer
  :group 'ert-flow)

(defcustom ert-flow-idle-gc-interval 30
  "Seconds between idle GC ticks checking sessions for auto-disabling watchers."
  :type 'integer
  :group 'ert-flow)

(defcustom ert-flow-max-raw-output-bytes 1048576
  "Maximum size (in characters) of accumulated raw output kept in memory per session.

When NIL, do not trim. To save memory, a value like 1 MiB (1048576) is recommended."
  :type '(choice (const :tag "No limit" nil) integer)
  :group 'ert-flow)

(defcustom ert-flow-copy-format 'plain
  "Format for copying failures: 'plain (default), 'org, or 'markdown."
  :type '(choice (const plain) (const org) (const markdown))
  :group 'ert-flow)

(defcustom ert-flow-copy-include-stdout nil
  "If non-nil, include the session's raw stdout tail in the copied failures block.

Length is capped by `ert-flow-copy-backtrace-limit' if non-nil."
  :type 'boolean
  :group 'ert-flow)

(defcustom ert-flow-copy-include-stderr nil
  "If non-nil, include the session's raw stderr tail in the copied failures block.

Length is capped by `ert-flow-copy-backtrace-limit' if non-nil."
  :type 'boolean
  :group 'ert-flow)

(defcustom ert-flow-session-naming-function
  (function ert-flow--default-session-name)
  "Function to produce a human-friendly session name from the project ROOT.

Signature: (fn root-string) → name-string."
  :type 'function
  :group 'ert-flow)

(defcustom ert-flow-copy-backtrace-limit nil
  "If non-nil, truncate each backtrace/details string to this many characters."
  :type '(choice (const :tag "No limit" nil) integer)
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

(defvar ert-flow--last-run-root nil
  "Absolute project root of the last started run. Used to route rendering to the correct panel.")

(defvar ert-flow--debounce-timer nil
  "Internal timer used to debounce auto-runs.")

(defvar ert-flow--watch-enabled nil
  "Non-nil when watch is enabled.")

(defvar ert-flow--file-notify-handles nil
  "List of file-notify descriptors for active watchers.")

(defvar ert-flow--active-after-save-count 0
  "Count of sessions with after-save watch enabled.")

(defvar ert-flow--active-run-count 0
  "Number of currently running test processes across all sessions.")

(defvar ert-flow--run-queue nil
  "Queue (FIFO) of pending runs as thunks (zero-arg lambdas).")

(defvar ert-flow--idle-gc-timer nil
  "Global timer that periodically auto-disables idle session watchers.")

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

(defun ert-flow--status-face (status)
  "Return face symbol for STATUS."
  (pcase status
    ('pass  'ert-flow-face-pass)
    ('fail  'ert-flow-face-fail)
    ('error 'ert-flow-face-error)
    ('skip  'ert-flow-face-skip)
    ('xfail 'ert-flow-face-skip)
    (_ 'default)))

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

(defun ert-flow--batch-pass1 (lines)
  "Scan LINES and collect name->status/details, totals and time.

Return a plist:
  (:name->status HASH :name->details HASH :all-names HASH
   :total TOTAL? :unexpected UNEXP? :time TIME-STR?)"
  (let ((name->status (make-hash-table :test 'equal))
        (name->details (make-hash-table :test 'equal))
        (all-names (make-hash-table :test 'equal))
        (total nil)
        (unexpected nil)
        (time-str nil))
    (cl-loop
     with i = 0
     while (< i (length lines))
     for line = (nth i lines)
     do
     (when (string-match "^Test[ \t]+\\([^ \t]+\\)[ \t]+passed\\.$" line)
       (let ((nm (match-string 1 line)))
         (puthash nm t all-names)
         (puthash nm 'pass name->status)))
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
    (list :name->status name->status
          :name->details name->details
          :all-names all-names
          :total total :unexpected unexpected :time time-str)))

(defun ert-flow--batch-pass2-final-status (lines name->status all-names)
  "Update NAME->STATUS and ALL-NAMES from final section in LINES.

Understands additional markers if present:
- XFAIL  → xfail (expected failure)
- XPASS  → fail  (unexpected pass is treated as failure)"
  (dolist (line lines)
    (when (string-match "^[ \t]*\\(FAILED\\|ERROR\\|SKIPPED\\|XFAIL\\|XPASS\\)[ \t]+\\(.+\\)$" line)
      (let* ((kw (match-string 1 line))
             (nm (ert-flow--string-trim (match-string 2 line)))
             (st (pcase kw
                   ("FAILED"  'fail)
                   ("ERROR"   'error)
                   ("SKIPPED" 'skip)
                   ("XFAIL"   'xfail)
                   ("XPASS"   'fail)
                   (_ 'fail))))
        (puthash nm t all-names)
        (puthash nm st name->status)))))

(defun ert-flow--batch-build-results (tables)
  "Build result plists from TABLES plist."
  (let* ((name->status (plist-get tables :name->status))
         (name->details (plist-get tables :name->details))
         (all-names (plist-get tables :all-names))
         results)
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
    (nreverse results)))

(defun ert-flow--batch-build-summary (time-str total unexpected results)
  "Compute summary alist from TIME-STR TOTAL UNEXPECTED RESULTS."
  (let* ((p (cl-count-if (lambda (r) (eq (plist-get r :status) 'pass)) results))
         (f (cl-count-if (lambda (r) (eq (plist-get r :status) 'fail)) results))
         (e (cl-count-if (lambda (r) (eq (plist-get r :status) 'error)) results))
         (s (cl-count-if (lambda (r) (memq (plist-get r :status) '(skip xfail))) results))
         (duration-ms (when (and time-str (string-match "\\`[0-9.]+\\'" time-str))
                        (truncate (* 1000 (string-to-number time-str))))))
    `((total . ,(or total (length results)))
      (unexpected . ,(or unexpected
                         (cl-count-if (lambda (r) (memq (plist-get r :status) '(fail error)))
                                      results)))
      (time . ,time-str)
      (duration-ms . ,duration-ms)
      (passed . ,p) (failed . ,f) (error . ,e) (skipped . ,s))))

(defun ert-flow--parse-batch-output (out)
  "Parse ERT batch OUT and return (SUMMARY . RESULTS). See helpers."
  (let* ((lines (split-string (or out "") "\n"))
         (t1 (ert-flow--batch-pass1 lines))
         (_ (ert-flow--batch-pass2-final-status
             lines
             (plist-get t1 :name->status)
             (plist-get t1 :all-names)))
         (results (ert-flow--batch-build-results t1))
         (summary (ert-flow--batch-build-summary
                   (plist-get t1 :time)
                   (plist-get t1 :total)
                   (plist-get t1 :unexpected)
                   results)))
    (cons summary results)))

;;;; JSON parsing and auto-dispatch

(defun ert-flow--json-safe-substring (s)
  "Return substring of S from first { to last }, or nil."
  (when (stringp s)
    (let ((start (string-match "{" s)))
      (when start
        (let ((end (cl-position ?} s :from-end t)))
          (when (and end (>= end start))
            (substring s start (1+ end))))))))

(defun ert-flow--json-parse (out)
  "Return parsed JSON object (alist) from OUT or signal on failure."
  (let* ((json-str (or (ert-flow--json-safe-substring out) out)))
    (json-parse-string json-str :object-type 'alist :array-type 'list)))

(defun ert-flow--json-tests (obj)
  "Extract tests array from OBJ, returning a list or nil."
  (let ((tests-raw (alist-get "tests" obj nil nil #'equal)))
    (cond
     ((null tests-raw) nil)
     ((listp tests-raw) tests-raw)
     ((vectorp tests-raw) (append tests-raw nil))
     (t (signal 'wrong-type-argument (list 'list tests-raw))))))

(defun ert-flow--json-test->plist (tobj)
  "Convert a single TOBJ (alist from JSON) into an ert-flow result plist."
  (let* ((nm (alist-get "name" tobj nil nil #'equal))
         (st-str (alist-get "status" tobj nil nil #'equal))
         (st (pcase (and st-str (downcase st-str))
               ("pass" 'pass) ("ok" 'pass)
               ("fail" 'fail) ("failed" 'fail)
               ("error" 'error)
               ("skip" 'skip) ("skipped" 'skip)
               ("xfail" 'xfail)
               (_ 'fail)))
         (msg (alist-get "message" tobj nil nil #'equal))
         (det (alist-get "details" tobj nil nil #'equal))
         (file (alist-get "file" tobj nil nil #'equal))
         (line (alist-get "line" tobj nil nil #'equal))
         (suite (ert-flow--suite-of nm)))
    (list :name nm
          :status st
          :message (or msg (and det (car (split-string det "\n" t))) (symbol-name st))
          :details (or det "")
          :suite suite
          :file (and file (format "%s" file))
          :line (and line (string-to-number (format "%s" line))))))

(defun ert-flow--json-build-summary (summary-raw results)
  "Build summary alist from SUMMARY-RAW (alist) and RESULTS (list)."
  (let* ((raw-total (and summary-raw (alist-get "total" summary-raw nil nil #'equal)))
         (total (cond
                 ((numberp raw-total) raw-total)
                 ((and (stringp raw-total) (string-match-p "\\`[0-9]+\\'" raw-total))
                  (string-to-number raw-total))
                 (t (length results))))
         (p (or (and summary-raw (alist-get "passed" summary-raw nil nil #'equal))
                (cl-count-if (lambda (r) (eq (plist-get r :status) 'pass)) results)))
         (f (or (and summary-raw (alist-get "failed" summary-raw nil nil #'equal))
                (cl-count-if (lambda (r) (eq (plist-get r :status) 'fail)) results)))
         (e (or (and summary-raw (alist-get "error" summary-raw nil nil #'equal))
                (cl-count-if (lambda (r) (eq (plist-get r :status) 'error)) results)))
         (s (or (and summary-raw (alist-get "skipped" summary-raw nil nil #'equal))
                (cl-count-if (lambda (r) (memq (plist-get r :status) '(skip xfail))) results)))
         (unexpected (seq-count (lambda (r) (memq (plist-get r :status) '(fail error))) results))
         (duration-ms
          (let ((dm (and summary-raw (alist-get "duration_ms" summary-raw nil nil #'equal)))
                (tstr (and summary-raw (alist-get "time" summary-raw nil nil #'equal))))
            (cond
             ((numberp dm) dm)
             ((and (stringp dm) (string-match-p "\\`[0-9]+\\'" dm)) (string-to-number dm))
             ((and (stringp tstr) (string-match-p "\\`[0-9.]+\\'" tstr))
              (truncate (* 1000 (string-to-number tstr))))
             (t nil))))
         (time-str (let ((tval (and summary-raw (alist-get "time" summary-raw nil nil #'equal))))
                     (and tval (format "%s" tval)))))
    `((total . ,total) (unexpected . ,unexpected)
      (time . ,time-str) (duration-ms . ,duration-ms)
      (passed . ,p) (failed . ,f) (error . ,e) (skipped . ,s))))

(defun ert-flow--parse-json-output (out)
  "Parse JSON OUT and return (SUMMARY . RESULTS) or nil on failure.

Expected keys:
- tests: array of {name,status,message?,details?,file?,line?,tags?}
- summary: {total,passed,failed,error,skipped,duration_ms?,time?}

Defensive parsing: accepts vectors or lists for arrays, tolerates missing keys."
  (condition-case _err
      (let* ((obj (ert-flow--json-parse out))
             (tests-raw (ert-flow--json-tests obj))
             (results (mapcar #'ert-flow--json-test->plist tests-raw))
             (summary-raw (alist-get "summary" obj nil nil #'equal))
             (summary (ert-flow--json-build-summary summary-raw results)))
        (cons summary results))
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

;;;; Concurrency and idle management

(defun ert-flow--touch-session (sess)
  "Mark SESS as active just now."
  (when sess
    (setf (ert-flow--session-last-activity-at sess) (current-time))))

(defun ert-flow--any-watch-enabled-p ()
  "Return non-nil if any session has watch enabled."
  (let (found)
    (maphash (lambda (_ s)
               (when (and (not found) (ert-flow--session-watch-enabled s))
                 (setq found t)))
             ert-flow--sessions)
    found))

(defun ert-flow--ensure-idle-gc-timer ()
  "Ensure the idle GC timer is running when any watch is enabled."
  (when (and (ert-flow--any-watch-enabled-p)
             (not (timerp ert-flow--idle-gc-timer)))
    (setq ert-flow--idle-gc-timer
          (run-at-time ert-flow-idle-gc-interval
                       ert-flow-idle-gc-interval
                       #'ert-flow--idle-gc-tick))))

(defun ert-flow--cancel-idle-gc-timer-if-unused ()
  "Cancel the idle GC timer if no sessions have watch enabled."
  (unless (ert-flow--any-watch-enabled-p)
    (when (timerp ert-flow--idle-gc-timer)
      (cancel-timer ert-flow--idle-gc-timer))
    (setq ert-flow--idle-gc-timer nil)))

(defun ert-flow--idle-gc-tick ()
  "Auto-disable watchers for idle sessions."
  (condition-case err
      (let ((now (current-time)))
        (maphash
         (lambda (_root s)
           (when (and (ert-flow--session-watch-enabled s)
                      (ert-flow--session-last-activity-at s))
             (let* ((idle (float-time (time-subtract now (ert-flow--session-last-activity-at s)))))
               (when (and (numberp idle)
                          (> idle ert-flow-session-idle-seconds))
                 (setf (ert-flow--session-watch-enabled s) nil)
                 (ert-flow--disable-watch s)
                 (ert-flow--log "idle-gc: disabled watch for %s after %.1fs"
                                (ert-flow--session-root s) idle)))))
         ert-flow--sessions)
        (ert-flow--cancel-idle-gc-timer-if-unused))
    (error (ert-flow--log "idle-gc error: %S" err))))

(defun ert-flow--enqueue-run (thunk)
  "Enqueue THUNK to run when a concurrency slot becomes available."
  (setq ert-flow--run-queue (append ert-flow--run-queue (list thunk))))

(defun ert-flow--dequeue-run ()
  "Dequeue and return next run thunk or nil."
  (let ((head (car ert-flow--run-queue)))
    (setq ert-flow--run-queue (cdr ert-flow--run-queue))
    head))

(defun ert-flow--maybe-start-run (sess cmd label)
  "Start CMD for SESS with LABEL respecting concurrency limit."
  (if (>= ert-flow--active-run-count ert-flow-max-concurrent-runs)
      (progn
        (ert-flow--log "queue: %s (limit %d reached)" label ert-flow-max-concurrent-runs)
        (ert-flow--enqueue-run (lambda () (ert-flow--start-run-internal sess cmd label)))
        (ert-flow-open-panel)
        (with-current-buffer (get-buffer-create (ert-flow--session-panel-name (ert-flow--session-root sess)))
          (let ((inhibit-read-only t))
            (goto-char (point-max))
            (insert (propertize (format "Queued: %s\n" label) 'face 'shadow)))))
    (cl-incf ert-flow--active-run-count)
    (ert-flow--start-run-internal sess cmd label)))

(defun ert-flow--start-run-internal (sess cmd label)
  "Actually start the external process CMD for SESS, annotating LABEL."
  (ert-flow--touch-session sess)
  (let* ((root (ert-flow--session-root sess))
         (panel (ert-flow--session-panel-name root)))
    (when (process-live-p (ert-flow--session-process sess))
      (ert-flow--log "Killing previous process...")
      (ignore-errors (kill-process (ert-flow--session-process sess))))
    (setf (ert-flow--session-last-raw-output sess) nil
          (ert-flow--session-last-stderr-output sess) nil)
    (ert-flow-open-panel)
    (with-current-buffer (get-buffer-create panel)
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert (propertize (format "Running: %s %S\n" label cmd) 'face 'shadow))))
    (let ((default-directory root))
      (let* ((stderr-buf (generate-new-buffer " *ert-flow-stderr*"))
             (p (make-process
                 :name "ert-flow-runner"
                 :command cmd
                 :noquery t
                 :buffer nil
                 :connection-type 'pipe
                 :filter #'ert-flow--proc-filter
                 :sentinel #'ert-flow--proc-sentinel
                 :stderr stderr-buf)))
        (process-put p 'ert-flow-session sess)
        (process-put p 'ert-flow-stderr-buf stderr-buf)
        (setf (ert-flow--session-process sess) p)
        ;; legacy global for compatibility
        (setq ert-flow--process p)))))

;;;; Internal run finish and in-Emacs runner

(defun ert-flow--finish-run ()
  "Bookkeeping after a run finishes: free a slot and start next queued run."
  (when (> ert-flow--active-run-count 0)
    (cl-decf ert-flow--active-run-count))
  (let ((next (ert-flow--dequeue-run)))
    (when next (funcall next))))

(defun ert-flow--maybe-start-thunk (sess label thunk)
  "Start THUNK respecting concurrency for SESS and LABEL."
  (if (>= ert-flow--active-run-count ert-flow-max-concurrent-runs)
      (progn
        (ert-flow--log "queue(thunk): %s (limit %d reached)" label ert-flow-max-concurrent-runs)
        (ert-flow--enqueue-run thunk)
        (ert-flow-open-panel)
        (with-current-buffer (get-buffer-create (ert-flow--session-panel-name (ert-flow--session-root sess)))
          (let ((inhibit-read-only t))
            (goto-char (point-max))
            (insert (propertize (format "Queued: %s\n" label) 'face 'shadow)))))
    (cl-incf ert-flow--active-run-count)
    (funcall thunk)))

(defun ert-flow--selector-names (names)
  "Return an ERT selector predicate that matches tests by NAMES (list of strings)."
  (lambda (test)
    (let* ((sym (ert-test-name test))
           (nm (if (symbolp sym) (symbol-name sym) (format "%s" sym))))
      (member nm names))))

;; In-Emacs runner helpers (enrichment without relying on batch parsing only)

(defun ert-flow--test-by-name (name)
  "Return ERT test object by NAME (string), or nil."
  (condition-case _err
      (let ((sym (intern-soft name)))
        (when (and sym (fboundp sym) (fboundp 'ert-get-test))
          (ert-get-test sym)))
    (error nil)))

(defun ert-flow--result-enrich-from-test (r test)
  "Return a copy of result plist R enriched with TEST metadata (file/line/tags)."
  (let ((res (copy-sequence r)))
    (when test
      (condition-case _e
          (when (fboundp 'ert-test-location)
            (pcase (ert-test-location test)
              (`(,file . ,line)
               (setq res (plist-put res :file (and file (format "%s" file))))
               (setq res (plist-put res :line (and line (numberp line) line))))))
        (error nil))
      (condition-case _e
          (let* ((tags (cond
                        ((fboundp 'ert-test-tags) (ert-test-tags test))
                        ((fboundp 'ert-test-properties)
                         (plist-get (ert-test-properties test) :tags))
                        (t nil))))
            (when (and tags (listp tags))
              (setq res (plist-put res :tags tags))))
        (error nil)))
    res))

(defun ert-flow--enrich-results-with-tests (results)
  "Enrich RESULTS list of plists with per-test metadata from loaded ERT tests."
  (mapcar (lambda (r)
            (let ((tobj (ert-flow--test-by-name (plist-get r :name))))
              (ert-flow--result-enrich-from-test r tobj)))
          results))

(defun ert-flow--summary-ensure-duration (summary start-time end-time)
  "Ensure SUMMARY alist has numeric duration in milliseconds using START-TIME/END-TIME."
  (let* ((dur-ms (alist-get 'duration-ms summary)))
    (if (numberp dur-ms)
        summary
      (let* ((elapsed (max 0.0 (float-time (time-subtract end-time start-time))))
             (ms (truncate (* 1000 elapsed)))
             (cell (assoc 'duration-ms summary)))
        (if cell
            (progn (setcdr cell ms) summary)
          (cons (cons 'duration-ms ms) summary))))))

(defun ert-flow--status-from-ert-result (result)
  "Map ERT RESULT object to ert-flow status symbol."
  (condition-case _e
      (pcase (and (fboundp 'ert-test-result-type)
                  (ert-test-result-type result))
        ('passed 'pass)
        ('failed 'fail)
        ('error  'error)
        ('skipped 'skip)
        (_ 'error))
    (error 'error)))

(defun ert-flow--details-from-ert-result (result)
  "Build a human-readable DETAILS string from ERT RESULT.
Includes condition and best-effort backtrace when available."
  (let ((details ""))
    (condition-case _e
        (when (and (fboundp 'ert-test-result-with-condition-condition)
                   (memq (ert-flow--status-from-ert-result result) '(fail error)))
          (let* ((cond (ert-test-result-with-condition-condition result))
                 (cond-str (format "Condition: %S" cond)))
            (setq details (concat details cond-str "\n"))))
      (error nil))
    (condition-case _e
        (when (and (fboundp 'ert-test-result-with-condition-backtrace)
                   (memq (ert-flow--status-from-ert-result result) '(fail error)))
          (let ((bt (ert-test-result-with-condition-backtrace result)))
            ;; Best-effort pretty-print of backtrace; falls back to prin1 if needed
            (setq details
                  (concat details
                          "Backtrace:\n"
                          (condition-case _pp
                              (with-output-to-string
                                (princ (format "%S" bt)))
                            (error (format "%S" bt)))
                          "\n"))))
      (error nil))
    (string-trim-right details)))

(defun ert-flow--result-from-ert (test result duration-ms)
  "Construct an ert-flow result plist from ERT TEST, RESULT and DURATION-MS."
  (let* ((sym (ert-test-name test))
         (name (if (symbolp sym) (symbol-name sym) (format "%s" sym)))
         (status (ert-flow--status-from-ert-result result))
         (details (ert-flow--details-from-ert-result result))
         (msg (if (and (stringp details) (> (length details) 0))
                  (car (split-string details "\n" t))
                (symbol-name status)))
         (file nil) (line nil) (tags nil))
    (condition-case _e
        (when (fboundp 'ert-test-location)
          (pcase (ert-test-location test)
            (`(,f . ,ln)
             (setq file (and f (format "%s" f)))
             (setq line (and (integerp ln) ln)))))
      (error nil))
    (condition-case _e
        (setq tags (cond
                    ((fboundp 'ert-test-tags) (ert-test-tags test))
                    ((fboundp 'ert-test-properties)
                     (plist-get (ert-test-properties test) :tags))
                    (t nil)))
      (error (setq tags nil)))
    (list :name name
          :status status
          :message msg
          :details (or details "")
          :suite (ert-flow--suite-of name)
          :file file
          :line line
          :tags tags
          :duration-ms (and (integerp duration-ms) duration-ms))))

(defun ert-flow--build-summary-from-results (results total-ms)
  "Compute a SUMMARY alist from RESULTS and TOTAL-MS duration."
  (let* ((total (length results))
         (p (cl-count-if (lambda (r) (eq (plist-get r :status) 'pass)) results))
         (f (cl-count-if (lambda (r) (eq (plist-get r :status) 'fail)) results))
         (e (cl-count-if (lambda (r) (eq (plist-get r :status) 'error)) results))
         (s (cl-count-if (lambda (r) (memq (plist-get r :status) '(skip xfail))) results))
         (unexpected (+ f e)))
    `((total . ,total)
      (unexpected . ,unexpected)
      (time . nil)
      (duration-ms . ,(and (integerp total-ms) total-ms))
      (passed . ,p) (failed . ,f) (error . ,e) (skipped . ,s))))

(defun ert-flow--collect-in-emacs-results (selector)
  "Run tests matching SELECTOR and return (SUMMARY . RESULTS).

This avoids parsing textual ERT output by inspecting result objects."
  (let* ((tests (ert-select-tests selector t))
         (results '())
         (suite-start (current-time)))
    (dolist (tobj tests)
      (let* ((start (current-time))
             (res (ert-run-test tobj))
             (elapsed (truncate (* 1000 (max 0.0 (float-time (time-subtract (current-time) start))))))
             (one (ert-flow--result-from-ert tobj res elapsed)))
        (push one results)))
    (let* ((total-ms (truncate (* 1000 (max 0.0 (float-time (time-subtract (current-time) suite-start))))))
           (results* (nreverse results))
           (summary (ert-flow--build-summary-from-results results* total-ms)))
      (cons summary results*))))

(defun ert-flow--start-in-emacs (sess selector label)
  "Run ERT SELECTOR inside Emacs, update SESS, and re-render panel. LABEL is informational.

Uses in-process ERT result objects (no text parsing) to gather precise
status, file/line, tags and backtraces."
  (unwind-protect
      (let* ((root (ert-flow--session-root sess))
             (panel (ert-flow--session-panel-name root))
             (pair (ert-flow--collect-in-emacs-results selector))
             (summary (car pair))
             (results (cdr pair)))
        ;; Store into session and legacy globals
        (setf (ert-flow--session-last-summary sess) summary
              (ert-flow--session-last-results sess) results
              (ert-flow--session-last-raw-output sess) nil)
        (setq ert-flow--last-summary summary
              ert-flow--last-results results
              ert-flow--last-raw-output nil)
        (let ((ert-flow--panel-buffer-name panel))
          (ert-flow--render)))
    (ert-flow--finish-run)))

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
    (define-key map (kbd "TAB") #'ert-flow-toggle-group-at-point)
    (define-key map (kbd "<backtab>") #'ert-flow-toggle-all-groups)
    map)
  "Keymap for `ert-flow-panel-mode'.")

(define-derived-mode ert-flow-panel-mode special-mode "ert-flow-panel"
  "Major mode for displaying ERT results in a side panel."
  (setq buffer-read-only t
        truncate-lines t)
  ;; Buffer-local fold state for suite groups
  (setq-local ert-flow--folded-suites (or ert-flow--folded-suites
                                          (make-hash-table :test 'equal))))

;;;###autoload
(defun ert-flow-open-panel ()
  "Open or focus the ert-flow panel (session-aware)."
  (interactive)
  (let* ((root (ert-flow--project-root))
         (sess (ert-flow--get-session root))
         (bufname (ert-flow--session-panel-name root))
         (buf (get-buffer-create bufname))
         (win (display-buffer-in-side-window
               buf
               `((side . ,(ert-flow--conf sess 'panel-side ert-flow-panel-side))
                 (window-width . ,(ert-flow--conf sess 'panel-width ert-flow-panel-width))))))
    (ert-flow--touch-session sess)
    (with-current-buffer buf
      (ert-flow-panel-mode)
      (let ((ert-flow--panel-buffer-name bufname))
        (ert-flow--render)))
    (select-window win)))

(defun ert-flow--propertize (text result)
  "Return TEXT with RESULT stored as a text property."
  (propertize text 'ert-flow--result result))

(defun ert-flow--toolbar-icon (key face &optional alt)
  "Return a toolbar icon string for KEY using FACE.
If all-the-icons is available and `ert-flow-toolbar-style' allows, return a glyph.
Otherwise return ALT (text/Unicode) or a reasonable fallback.

Known KEYs: 'run, 'run-failed, 'watch, 'copy, 'clear, 'detect, 'goto."
  (let* ((icons-ok (and (memq ert-flow-toolbar-style '(auto icons))
                        (featurep 'all-the-icons)
                        (fboundp 'all-the-icons-material)))
         (glyph
          (when icons-ok
            (pcase key
              ('run        (all-the-icons-material "play_arrow" :face face))
              ('run-failed (all-the-icons-material "replay" :face face))
              ('watch      (all-the-icons-material "visibility" :face face))
              ('copy       (all-the-icons-material "content_copy" :face face))
              ('clear      (all-the-icons-material "clear" :face face))
              ('detect     (all-the-icons-material "search" :face face))
              ('goto       (all-the-icons-material "open_in_new" :face face))
              (_ nil)))))
    (or glyph
        (propertize (or alt "") 'face face))))

(defun ert-flow--insert-toolbar (sess)
  "Insert a colored, clickable toolbar into current buffer for SESS."
  (let ((inhibit-read-only t))
    (cl-labels
        ((btn (label key face cmd icon-alt help)
           (let* ((icon (ert-flow--toolbar-icon key face icon-alt))
                  (text (format " %s %s " icon label)))
             (insert-text-button
              text
              'face face
              'mouse-face 'highlight
              'help-echo help
              'follow-link t
              'action (lambda (_btn) (call-interactively cmd))))))
      (let* ((runner (ert-flow--conf sess 'runner ert-flow-runner))
             (ext-cmd (ert-flow--conf sess 'external-command ert-flow-external-command))
             (failed-fn (ert-flow--conf sess 'external-failed-args-function ert-flow-external-failed-args-function)))
        (btn "Run"       'run        'ert-flow-toolbar-run        #'ert-flow-run       "▶"
             (format "Run tests via %s (r)" (if (eq runner 'in-emacs-ert) "in-Emacs" "external")))
        (insert " ")
        (let ((help-f (if (eq runner 'in-emacs-ert)
                          "Run failed tests in-Emacs (ERT selector) (f)"
                        (if (and (listp ext-cmd) failed-fn)
                            "Run failed tests (external argv via failed-args) (f)"
                          "Run failed tests (falls back to all) (f)"))))
          (btn "Failed"    'run-failed 'ert-flow-toolbar-run-failed #'ert-flow-run-failed "↻" help-f))
        (insert " ")
        (btn (format "Watch:%s" (if (and sess (ert-flow--session-watch-enabled sess)) "On" "Off"))
             'watch 'ert-flow-toolbar-watch #'ert-flow-toggle-watch "👁" "Toggle watch (w)")
        (insert " ")
        (btn "Copy"      'copy       'ert-flow-toolbar-copy       #'ert-flow-copy-failures "⧉" "Copy failures (c)")
        (insert " ")
        (btn "Clear"     'clear      'ert-flow-toolbar-clear      #'ert-flow-clear "✖" "Clear panel (x)")
        (insert " ")
        (btn "Detect"    'detect     'ert-flow-toolbar-detect     #'ert-flow-detect-runner "🔎" "Detect runner (d)")
        (insert " ")
        (btn "Goto"      'goto       'ert-flow-toolbar-goto       #'ert-flow-goto-definition-at-point "↗" "Goto test definition (o)")
        (insert "\n")))))

(defun ert-flow--find-panel-session ()
  "Return session object for the current `ert-flow--panel-buffer-name'."
  (let (found)
    (maphash
     (lambda (_root s)
       (when (and (not found)
                  (string= (ert-flow--session-panel-buf-name s)
                           ert-flow--panel-buffer-name))
         (setq found s)))
     ert-flow--sessions)
    (or found (ert-flow--get-session (ert-flow--project-root)))))

(defun ert-flow--insert-header-line (sess)
  "Insert title, toolbar and context line for SESS."
  (insert (propertize "ert-flow  " 'face 'mode-line-buffer-id))
  (ert-flow--insert-toolbar sess)
  (insert (format "Project: %s | Runner: %s | Mode: %s | Watch: %s\n"
                  (file-name-nondirectory (directory-file-name (ert-flow--session-root sess)))
                  (if (eq (ert-flow--conf sess 'runner ert-flow-runner) 'in-emacs-ert) "in-emacs" "external")
                  (ert-flow--conf sess 'watch-mode ert-flow-watch-mode)
                  (if (and sess (ert-flow--session-watch-enabled sess)) "On" "Off"))))

(defun ert-flow--summary-counters (sum results)
  "Compute counters and duration fields from SUM/RESULTS."
  (let* ((total (or (alist-get 'total sum) 0))
         (unexpected (or (alist-get 'unexpected sum) 0))
         (p (or (alist-get 'passed sum)
                (cl-count-if (lambda (r) (eq (plist-get r :status) 'pass)) results)))
         (f (or (alist-get 'failed sum)
                (cl-count-if (lambda (r) (eq (plist-get r :status) 'fail)) results)))
         (e (or (alist-get 'error sum)
                (cl-count-if (lambda (r) (eq (plist-get r :status) 'error)) results)))
         (s (or (alist-get 'skipped sum)
                (cl-count-if (lambda (r) (memq (plist-get r :status) '(skip xfail))) results)))
         (time-str (alist-get 'time sum))
         (dur-ms (alist-get 'duration-ms sum))
         (dur-str (cond
                   ((numberp dur-ms) (format "%.3fs" (/ dur-ms 1000.0)))
                   (time-str (format "%ss" time-str))
                   (t "-"))))
    (list :total total :unexpected unexpected :passed p :failed f :error e :skipped s
          :dur-str dur-str)))

(defun ert-flow--insert-summary-line (sum results)
  "Insert summary line using SUM and RESULTS."
  (cl-destructuring-bind (&key total unexpected passed failed error skipped dur-str)
      (ert-flow--summary-counters sum results)
    (let* ((u-face (if (> unexpected 0) 'ert-flow-face-fail 'ert-flow-face-pass))
           (active ert-flow--active-run-count)
           (queued (length ert-flow--run-queue)))
      (insert "Summary: ")
      (insert (format "%d (" total))
      (insert (propertize (format "P:%d" passed) 'face 'ert-flow-face-pass))
      (insert " ")
      (insert (propertize (format "F:%d" failed) 'face 'ert-flow-face-fail))
      (insert " ")
      (insert (propertize (format "E:%d" error) 'face 'ert-flow-face-error))
      (insert " ")
      (insert (propertize (format "S:%d" skipped) 'face 'ert-flow-face-skip))
      (insert ")  unexpected: ")
      (insert (propertize (format "%d" unexpected) 'face u-face))
      (insert (format "  duration: %s  | Proc: active %d, queued %d\n\n"
                      dur-str active queued))
      (insert (propertize "Tip: click a test for details; press 'o' to jump to definition. TAB folds groups.\n\n" 'face 'shadow)))))

(defun ert-flow--group-results (results)
  "Return alist (SUITE . LIST-OF-RESULTS) from RESULTS."
  (let ((ht (make-hash-table :test 'equal))
        acc)
    (dolist (r results)
      (let* ((suite (or (plist-get r :suite) ""))
             (bucket (gethash suite ht)))
        (puthash suite (cons r bucket) ht)))
    (maphash (lambda (k v) (push (cons k (nreverse v)) acc)) ht)
    (sort acc (lambda (a b) (string< (car a) (car b))))))

(defun ert-flow--insert-test-line (r)
  "Insert single test line for result plist R with properties."
  (let* ((st (plist-get r :status))
         (nm (plist-get r :name))
         (icon (if ert-flow-icons (ert-flow--status-icon st) ""))
         (face (ert-flow--status-face st))
         (line (format "  %s %s\n"
                       (if (> (length icon) 0)
                           (propertize icon 'face face)
                         "")
                       nm)))
    (insert (ert-flow--propertize line r))))

(defun ert-flow--insert-suite (suite results)
  "Insert a SUITE heading and its RESULTS, respecting fold state."
  (let* ((folded (and (boundp 'ert-flow--folded-suites)
                      ert-flow--folded-suites
                      (gethash suite ert-flow--folded-suites)))
         (arrow (if folded "▸" "▾")))
    (insert (propertize (format "%s %s\n" arrow suite)
                        'face 'bold
                        'ert-flow--suite suite))
    (unless folded
      (dolist (r results)
        (ert-flow--insert-test-line r)))))

(defun ert-flow--render ()
  "Render the panel for the session associated with `ert-flow--panel-buffer-name'."
  (let ((buf (get-buffer-create ert-flow--panel-buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (let* ((sess (ert-flow--find-panel-session))
               (sum (or (and sess (ert-flow--session-last-summary sess)) ert-flow--last-summary))
               (results (or (and sess (ert-flow--session-last-results sess)) ert-flow--last-results)))
          (ert-flow--insert-header-line sess)
          (ert-flow--insert-summary-line sum results)
          (dolist (pair (ert-flow--group-results results))
            (ert-flow--insert-suite (car pair) (cdr pair))))
        (goto-char (point-min))))))


(defun ert-flow-open-details-at-point ()
  "Open a buffer with details for the test at point (session-aware)."
  (interactive)
  (let* ((r (get-text-property (line-beginning-position) 'ert-flow--result)))
    (if (null r)
        (user-error "No test result at point")
      (let* ((root (ert-flow--project-root))
             (bufname (ert-flow--session-details-name root))
             (buf (get-buffer-create bufname))
             (name (plist-get r :name))
             (details (or (plist-get r :details) "No details")))
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (read-only-mode -1)
            (erase-buffer)
            ;; Toolbar
            (insert (propertize (format "%s  " name) 'face 'mode-line-buffer-id))
            (insert-text-button "[Goto]"
                                'face 'link
                                'mouse-face 'highlight
                                'help-echo "Goto test definition (o)"
                                'follow-link t
                                'action (lambda (_)
                                          (ignore-errors
                                            (let ((sym (intern-soft name)))
                                              (when (and sym (fboundp sym))
                                                (find-function sym))))))
            (insert "  ")
            (insert-text-button "[Copy details]"
                                'face 'link
                                'mouse-face 'highlight
                                'help-echo "Copy details block"
                                'follow-link t
                                'action (lambda (_)
                                          (kill-new details)
                                          (message "ert-flow: details copied")))
            (insert "  ")
            (insert-text-button "[Close]"
                                'face 'link
                                'mouse-face 'highlight
                                'help-echo "Close window"
                                'follow-link t
                                'action (lambda (_)
                                          (quit-window t)))
            (insert "\n\n")
            ;; Body
            (insert details)
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
  "Run only failed/error tests if possible, else run all (session-aware).

For 'external-command runner:
- Uses session's `external-failed-args-function' to build extra argv.

For 'in-emacs-ert runner:
- Builds an ERT selector for failed test names and runs inside Emacs."
  (interactive)
  (let* ((root (ert-flow--project-root))
         (sess (ert-flow--get-session root))
         (results (or (and sess (ert-flow--session-last-results sess)) ert-flow--last-results))
         (fails
          (mapcar (lambda (r) (plist-get r :name))
                  (seq-filter (lambda (r) (memq (plist-get r :status) '(fail error)))
                              (or results '()))))
         (runner (ert-flow--conf sess 'runner ert-flow-runner))
         (ext-cmd (ert-flow--conf sess 'external-command ert-flow-external-command))
         (failed-fn (ert-flow--conf sess 'external-failed-args-function ert-flow-external-failed-args-function)))
    (cond
     ((null fails)
      (message "ert-flow: no failed tests to run")
      (ert-flow-run))
     ((eq runner 'in-emacs-ert)
      (let ((selector (ert-flow--selector-names fails))
            (label (format "failed(in-emacs) %d" (length fails))))
        (ert-flow--maybe-start-thunk
         sess label
         (lambda () (ert-flow--start-in-emacs sess selector label)))))
     (t
      (if (or (null failed-fn)
              (not (listp ext-cmd)))
          (progn
            (message "ert-flow: cannot run failed selectively; running all")
            (ert-flow-run))
        (let* ((extra (ignore-errors (funcall failed-fn fails))))
          (if (not (and (listp extra) (seq-every-p #'stringp extra)))
              (progn
                (message "ert-flow: bad failed-args; running all")
                (ert-flow-run))
            (let ((cmd (append ext-cmd extra)))
              (ert-flow--maybe-start-run sess cmd "failed")))))))))

;;;###autoload
(defun ert-flow-run ()
  "Run tests according to per-session runner (session-aware).

- 'external-command: uses session's external-command
- 'in-emacs-ert: runs `ert-run-tests-batch' inside Emacs and parses output"
  (interactive)
  (let* ((root (ert-flow--project-root))
         (sess (ert-flow--get-session root))
         (runner (ert-flow--conf sess 'runner ert-flow-runner)))
    (pcase runner
      ('in-emacs-ert
       (let ((selector t) (label "all(in-emacs)"))
         (ert-flow--maybe-start-thunk
          sess label
          (lambda () (ert-flow--start-in-emacs sess selector label)))))
      (_
       (let* ((ext (ert-flow--conf sess 'external-command ert-flow-external-command))
              (cmd (ert-flow--normalize-command ext)))
         (unless cmd
           (user-error "Set per-session external command (M-x ert-flow-detect-runner)"))
         (ert-flow--maybe-start-run sess cmd "all"))))))

(defun ert-flow--proc-filter (proc chunk)
  "Accumulate CHUNK from the running process, session-aware."
  (condition-case err
      (let* ((sess (process-get proc 'ert-flow-session))
             (old (and sess (ert-flow--session-last-raw-output sess)))
             (combined (concat (or old "") chunk)))
        ;; Trim per configured cap to save memory
        (when (and (integerp ert-flow-max-raw-output-bytes)
                   (> (length combined) ert-flow-max-raw-output-bytes))
          (setq combined (substring combined (- (length combined) ert-flow-max-raw-output-bytes))))
        (when sess
          (setf (ert-flow--session-last-raw-output sess) combined))
        ;; Legacy global mirrors current session
        (setq ert-flow--last-raw-output combined))
    (error
     (ert-flow--log "Filter error: %S" err))))

(defun ert-flow--proc-sentinel (proc event)
  "Handle process EVENT (session-aware)."
  (condition-case err
      (progn
        (ert-flow--log "Sentinel: %s" (string-trim (or event "")))
        (when (and (stringp event)
                   (string-match-p "\\(finished\\|exited\\)" event))
          (let* ((sess (process-get proc 'ert-flow-session))
                 (root (and sess (ert-flow--session-root sess)))
                 (raw  (or (and sess (ert-flow--session-last-raw-output sess))
                           ert-flow--last-raw-output))
                 ;; collect stderr (if any)
                 (stderr-buf (process-get proc 'ert-flow-stderr-buf))
                 (stderr-str (when (buffer-live-p stderr-buf)
                               (with-current-buffer stderr-buf
                                 (buffer-string))))
                 (parsed (let ((ert-flow-parser (ert-flow--conf sess 'parser ert-flow-parser)))
                           (ert-flow--parse-output raw)))
                 (summary (car parsed))
                 (results (cdr parsed)))
            ;; trim stderr if needed and store in session
            (when sess
              (when (and (stringp stderr-str)
                         (integerp ert-flow-max-raw-output-bytes)
                         (> (length stderr-str) ert-flow-max-raw-output-bytes))
                (setq stderr-str (substring stderr-str (- (length stderr-str) ert-flow-max-raw-output-bytes))))
              (setf (ert-flow--session-last-stderr-output sess) stderr-str))
            ;; cleanup stderr buffer
            (when (buffer-live-p stderr-buf)
              (kill-buffer stderr-buf))
            (ert-flow--touch-session sess)
            ;; Update session state
            (when sess
              (setf (ert-flow--session-last-summary sess) summary
                    (ert-flow--session-last-results sess) results
                    (ert-flow--session-process sess) nil))
            ;; Legacy globals for compatibility
            (setq ert-flow--last-summary summary
                  ert-flow--last-results results)
            ;; Re-render the correct panel
            (let* ((bufname (ert-flow--session-panel-name (or root (ert-flow--project-root)))))
              (let ((ert-flow--panel-buffer-name bufname))
                (ert-flow--render)))
            ;; Concurrency bookkeeping and next queued run.
            (ert-flow--finish-run))))
    (error
     (ert-flow--log "Sentinel error: %S" err))))

;;;; Runner helpers

(defun ert-flow--ensure-fold-table ()
  "Ensure the panel buffer has a fold table initialized."
  (unless (hash-table-p ert-flow--folded-suites)
    (setq-local ert-flow--folded-suites (make-hash-table :test 'equal))))

;;;###autoload
(defun ert-flow-toggle-group-at-point ()
  "Toggle folding of the suite group at point."
  (interactive)
  (ert-flow--ensure-fold-table)
  (let* ((suite (or (get-text-property (line-beginning-position) 'ert-flow--suite)
                    (save-excursion
                      (beginning-of-line)
                      (when (re-search-backward "^" nil t)
                        (get-text-property (point) 'ert-flow--suite))))))
    (unless suite
      (user-error "No suite group at point"))
    (let ((cur (gethash suite ert-flow--folded-suites)))
      (puthash suite (not cur) ert-flow--folded-suites))
    (ert-flow--render)))

;;;###autoload
(defun ert-flow-toggle-all-groups (&optional expand)
  "Toggle folding for all groups. With EXPAND non-nil, unfold all."
  (interactive "P")
  (ert-flow--ensure-fold-table)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (let ((suite (get-text-property (point) 'ert-flow--suite)))
        (when suite
          (puthash suite (not expand) ert-flow--folded-suites)))
      (forward-line 1)))
  (ert-flow--render))

;;;###autoload
(defun ert-flow-detect-runner ()
  "Detect a suitable external command for running tests and set it per-session.

Heuristics (in order):
- tests/run-tests.el or test/run-tests.el → emacs -Q --batch -l <path>
- flake.nix present → nix run .#tests
- Cask present → cask exec ert-runner

If multiple candidates are available, prompt to choose."
  (interactive)
  (let* ((root (ert-flow--project-root))
         (sess (ert-flow--get-session root))
         (json (expand-file-name "tests/run-tests.el" root))
         (json2 (expand-file-name "test/run-tests.el" root))
         (flake (expand-file-name "flake.nix" root))
         (cask  (expand-file-name "Cask" root))
         (cands nil))
    (when (file-exists-p json)
      (push (cons "emacs -Q --batch -l tests/run-tests.el"
                  (list "emacs" "-Q" "--batch" "-l" json))
            cands))
    (when (and (not (file-exists-p json)) (file-exists-p json2))
      (push (cons "emacs -Q --batch -l test/run-tests.el"
                  (list "emacs" "-Q" "--batch" "-l" json2))
            cands))
    (when (file-exists-p flake)
      (push (cons "nix run .#tests"
                  (list "nix" "run" ".#tests"))
            cands))
    (when (file-exists-p cask)
      ;; Common Cask-based runner; depends on ert-runner being configured
      (push (cons "cask exec ert-runner"
                  (list "cask" "exec" "ert-runner"))
            cands))
    (cond
     ((null cands)
      (user-error "ert-flow: no known test entrypoint found under %s" root))
     ((= (length cands) 1)
      (ert-flow--set-conf sess 'external-command (cdar cands))
      ;; keep global in sync as a fallback
      (setq ert-flow-external-command (cdar cands))
      (message "ert-flow: session external command set to %S" (cdar cands)))
     (t
      (let* ((choice (completing-read "Choose runner: " (mapcar #'car cands) nil t))
             (cmd (cdr (assoc choice cands))))
        (ert-flow--set-conf sess 'external-command cmd)
        (setq ert-flow-external-command cmd)
        (message "ert-flow: session external command set to %S" cmd))))))

;;;; Sessions

(defvar ert-flow--sessions (make-hash-table :test 'equal)
  "Registry of ert-flow sessions keyed by project root (absolute path).")

(cl-defstruct ert-flow--session
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
  last-activity-at)

;; Per-session configuration

(defun ert-flow--dir-locals-snapshot (root symbols)
  "Return alist (SYMBOL . VALUE) of dir-local values for SYMBOLS under ROOT.
Creates a temporary buffer visiting a file in ROOT to read .dir-locals."
  (let ((buf (generate-new-buffer " *ert-flow dirlocals*")))
    (unwind-protect
        (with-current-buffer buf
          (setq default-directory (file-name-as-directory (expand-file-name root)))
          (setq-local buffer-file-name (expand-file-name "._ert_flow_probe_.el" default-directory))
          (hack-dir-local-variables)
          (mapcar (lambda (sym) (cons sym (buffer-local-value sym buf))) symbols))
      (kill-buffer buf))))

(defun ert-flow--init-session-config (root)
  "Build initial configuration alist for session at ROOT.

Keys include:
  runner parser external-command external-failed-args-function
  watch-mode debounce-seconds watch-include-regexp watch-exclude-regexp
  file-notify-max-depth panel-side panel-width icons toolbar-style"
  (let* ((mapping '((runner                      . ert-flow-runner)
                    (parser                      . ert-flow-parser)
                    (external-command            . ert-flow-external-command)
                    (external-failed-args-function . ert-flow-external-failed-args-function)
                    (watch-mode                  . ert-flow-watch-mode)
                    (debounce-seconds            . ert-flow-debounce-seconds)
                    (watch-include-regexp        . ert-flow-watch-include-regexp)
                    (watch-exclude-regexp        . ert-flow-watch-exclude-regexp)
                    (file-notify-max-depth       . ert-flow-file-notify-max-depth)
                    (panel-side                  . ert-flow-panel-side)
                    (panel-width                 . ert-flow-panel-width)
                    (icons                       . ert-flow-icons)
                    (toolbar-style               . ert-flow-toolbar-style)))
         (symbols (mapcar #'cdr mapping))
         (dirvals (condition-case _ (ert-flow--dir-locals-snapshot root symbols) (error nil)))
         (alist nil))
    (dolist (pair mapping)
      (let* ((key (car pair))
             (var (cdr pair))
             (val (if (and dirvals (assq var dirvals))
                      (cdr (assq var dirvals))
                    (symbol-value var))))
        (push (cons key val) alist)))
    (nreverse alist)))

(defun ert-flow--conf (sess key default)
  "Get per-session configuration value for KEY from SESS or DEFAULT."
  (let* ((cfg (and sess (ert-flow--session-config sess)))
         (cell (assq key cfg)))
    (if cell (cdr cell) default)))

(defun ert-flow--set-conf (sess key value)
  "Set per-session configuration KEY to VALUE in SESS."
  (let* ((cfg (or (ert-flow--session-config sess) '()))
         (cell (assq key cfg)))
    (if cell
        (setcdr cell value)
      (push (cons key value) cfg))
    (setf (ert-flow--session-config sess) cfg)))

(defvar-local ert-flow--folded-suites nil
  "Hash-table of folded suite names (buffer-local in panel buffers).")

(defun ert-flow--default-session-name (root)
  "Return default human-friendly session name for ROOT."
  (file-name-nondirectory (directory-file-name root)))

(defun ert-flow--session-panel-name (root)
  "Return panel buffer name for project ROOT."
  (let ((name (funcall ert-flow-session-naming-function root)))
    (format "*ert-flow: %s*" name)))

(defun ert-flow--session-details-name (root)
  "Return details buffer name for project ROOT."
  (let ((name (funcall ert-flow-session-naming-function root)))
    (format "*ert-flow: %s: details*" name)))

(defun ert-flow--get-session (&optional root)
  "Get or create a session for ROOT (current project if nil)."
  (let* ((r (or root (ert-flow--project-root)))
         (abs (file-name-as-directory (expand-file-name r))))
    (or (gethash abs ert-flow--sessions)
        (let* ((sess (make-ert-flow--session
                      :root abs
                      :panel-buf-name (ert-flow--session-panel-name abs)
                      :details-buf-name (ert-flow--session-details-name abs)
                      :last-raw-output nil
                      :last-stderr-output nil
                      :last-results nil
                      :last-summary nil
                      :process nil
                      :debounce-timer nil
                      :watch-enabled nil
                      :file-notify-handles nil
                      :config (ert-flow--init-session-config abs))))
          (puthash abs sess ert-flow--sessions)
          sess))))

(defun ert-flow--session-list ()
  "Return a list of current sessions."
  (let (acc)
    (maphash (lambda (_ s) (push s acc)) ert-flow--sessions)
    acc))

;;;###autoload
(defun ert-flow-switch-session ()
  "Switch focus to another ert-flow session's panel."
  (interactive)
  (let* ((pairs (let (acc)
                  (maphash
                   (lambda (_root s)
                     (push (cons (ert-flow--session-panel-buf-name s)
                                 (ert-flow--session-root s))
                           acc))
                   ert-flow--sessions)
                  (nreverse acc))))
    (if (null pairs)
        (user-error "ert-flow: no sessions")
      (let* ((choice (completing-read "Switch to session: " (mapcar #'car pairs) nil t))
             (root (cdr (assoc choice pairs))))
        (let ((buf (get-buffer (ert-flow--session-panel-name root))))
          (unless buf (user-error "ert-flow: panel not found for %s" root))
          (let* ((sess (ert-flow--get-session root))
                 (side (ert-flow--conf sess 'panel-side ert-flow-panel-side))
                 (width (ert-flow--conf sess 'panel-width ert-flow-panel-width))
                 (win (display-buffer-in-side-window
                       buf
                       `((side . ,side)
                         (window-width . ,width)))))
            (select-window win)))))))

;;;###autoload
(defun ert-flow-kill-session (&optional root)
  "Kill ert-flow session for ROOT (current project if nil).

Stops watcher and process, cancels timers, and removes the session from registry."
  (interactive)
  (let* ((r (or root (ert-flow--project-root)))
         (sess (gethash r ert-flow--sessions)))
    (unless sess
      (user-error "ert-flow: no session for %s" r))
    ;; Stop process
    (when (process-live-p (ert-flow--session-process sess))
      (ignore-errors (kill-process (ert-flow--session-process sess))))
    ;; Disable watch
    (when (ert-flow--session-watch-enabled sess)
      (ert-flow--disable-watch sess))
    ;; Cancel debounce
    (when (timerp (ert-flow--session-debounce-timer sess))
      (cancel-timer (ert-flow--session-debounce-timer sess)))
    ;; Remove from registry
    (remhash r ert-flow--sessions)
    (message "ert-flow: killed session %s" r)))

;;;###autoload
(defun ert-flow-kill-all-sessions ()
  "Kill all ert-flow sessions."
  (interactive)
  (maphash (lambda (r _s) (ignore-errors (ert-flow-kill-session r))) ert-flow--sessions)
  (clrhash ert-flow--sessions)
  (message "ert-flow: killed all sessions"))

;;;###autoload
(defun ert-flow-list-sessions ()
  "List current sessions in a temporary buffer with quick actions.

Each row shows:
- panel name
- runner and watch mode
- watch On/Off and process state
- buttons: [Open] [Watch On/Off] [Kill]"
  (interactive)
  (let ((buf (get-buffer-create "*ert-flow: sessions*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (special-mode)
        (insert (propertize "ert-flow sessions\n\n" 'face 'bold))
        (if (= (hash-table-count ert-flow--sessions) 0)
            (insert "No active sessions.\n")
          (maphash
           (lambda (_root s)
             (let* ((root (ert-flow--session-root s))
                    (panel (ert-flow--session-panel-buf-name s))
                    (watch (if (ert-flow--session-watch-enabled s) "On" "Off"))
                    (runner (ert-flow--conf s 'runner ert-flow-runner))
                    (watch-mode (ert-flow--conf s 'watch-mode ert-flow-watch-mode))
                    (proc (and (process-live-p (ert-flow--session-process s)) "active")))
               (insert (propertize (format "- %s\n" panel) 'face 'bold))
               (insert (format "  Runner: %s | Mode: %s | Watch: %s | Proc: %s\n"
                               (if (eq runner 'in-emacs-ert) "in-emacs" "external")
                               watch-mode watch (or proc "idle")))
               ;; Buttons
               (insert "  ")
               (insert-text-button "[Open]"
                                   'face 'link
                                   'mouse-face 'highlight
                                   'help-echo "Open panel"
                                   'follow-link t
                                   'action (lambda (_)
                                             (let* ((buf (get-buffer (ert-flow--session-panel-name root)))
                                                    (sess (ert-flow--get-session root))
                                                    (side (ert-flow--conf sess 'panel-side ert-flow-panel-side))
                                                    (width (ert-flow--conf sess 'panel-width ert-flow-panel-width)))
                                               (unless buf
                                                 (setq buf (get-buffer-create (ert-flow--session-panel-name root))))
                                               (display-buffer-in-side-window
                                                buf `((side . ,side) (window-width . ,width))))))
               (insert "  ")
               (insert-text-button (format "[Watch %s]" (if (ert-flow--session-watch-enabled s) "Off" "On"))
                                   'face 'link
                                   'mouse-face 'highlight
                                   'help-echo "Toggle watch for this session"
                                   'follow-link t
                                   'action (lambda (_)
                                             (let ((default-directory root))
                                               (ert-flow-toggle-watch)
                                               (ert-flow-list-sessions))))
               (insert "  ")
               (insert-text-button "[Kill]"
                                   'face 'link
                                   'mouse-face 'highlight
                                   'help-echo "Kill this session"
                                   'follow-link t
                                   'action (lambda (_)
                                             (ert-flow-kill-session root)
                                             (ert-flow-list-sessions)))
               (insert "\n\n")))
           ert-flow--sessions))))
    (display-buffer buf)))

;;;###autoload
(defun ert-flow-dashboard ()
  "Show a summary dashboard across all ert-flow sessions with quick actions.

Displays:
- Global active/queued process counts
- Per-session: project name, runner, watch mode/state, last summary counters
- Buttons: [Open] [Run] [Watch On/Off] [Kill]

This view is read-only and uses text buttons for actions."
  (interactive)
  (let ((buf (get-buffer-create "*ert-flow: dashboard*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (special-mode)
        (insert (propertize "ert-flow dashboard\n\n" 'face 'bold))
        (insert (format "Processes: active %d, queued %d\n"
                        ert-flow--active-run-count
                        (length ert-flow--run-queue)))
        (insert (format "Sessions: %d\n\n" (hash-table-count ert-flow--sessions)))
        (if (= (hash-table-count ert-flow--sessions) 0)
            (insert "No active sessions.\n")
          (maphash
           (lambda (_root s)
             (let* ((root (ert-flow--session-root s))
                    (panel (ert-flow--session-panel-buf-name s))
                    (sess-name (file-name-nondirectory (directory-file-name root)))
                    (watch-on (ert-flow--session-watch-enabled s))
                    (runner (ert-flow--conf s 'runner ert-flow-runner))
                    (watch-mode (ert-flow--conf s 'watch-mode ert-flow-watch-mode))
                    (proc (and (process-live-p (ert-flow--session-process s)) "active"))
                    (sum (ert-flow--session-last-summary s))
                    (p (or (and sum (alist-get 'passed sum)) 0))
                    (f (or (and sum (alist-get 'failed sum)) 0))
                    (e (or (and sum (alist-get 'error sum)) 0))
                    (sk (or (and sum (alist-get 'skipped sum)) 0))
                    (tot (or (and sum (alist-get 'total sum)) 0)))
               (insert (propertize (format "- %s (%s)\n" panel sess-name) 'face 'bold))
               (insert (format "  Runner: %s | Mode: %s | Watch: %s | Proc: %s\n"
                               (if (eq runner 'in-emacs-ert) "in-emacs" "external")
                               watch-mode (if watch-on "On" "Off") (or proc "idle")))
               (insert (format "  Summary: total %d (P:%d F:%d E:%d S:%d)\n" tot p f e sk))
               ;; Buttons
               (insert "  ")
               (insert-text-button "[Open]"
                                   'face 'link 'mouse-face 'highlight 'follow-link t
                                   'help-echo "Open panel"
                                   'action (lambda (_)
                                             (let* ((buf (get-buffer (ert-flow--session-panel-name root)))
                                                    (sess (ert-flow--get-session root))
                                                    (side (ert-flow--conf sess 'panel-side ert-flow-panel-side))
                                                    (width (ert-flow--conf sess 'panel-width ert-flow-panel-width)))
                                               (unless buf
                                                 (setq buf (get-buffer-create (ert-flow--session-panel-name root))))
                                               (display-buffer-in-side-window
                                                buf `((side . ,side) (window-width . ,width))))))
               (insert "  ")
               (insert-text-button "[Run]"
                                   'face 'link 'mouse-face 'highlight 'follow-link t
                                   'help-echo "Run tests for this session"
                                   'action (lambda (_)
                                             (let ((default-directory root))
                                               (ert-flow-run)
                                               (message "ert-flow: scheduled run for %s" root))))
               (insert "  ")
               (insert-text-button (format "[Watch %s]" (if watch-on "Off" "On"))
                                   'face 'link 'mouse-face 'highlight 'follow-link t
                                   'help-echo "Toggle watch"
                                   'action (lambda (_)
                                             (let ((default-directory root))
                                               (ert-flow-toggle-watch)
                                               (ert-flow-dashboard))))
               (insert "  ")
               (insert-text-button "[Kill]"
                                   'face 'link 'mouse-face 'highlight 'follow-link t
                                   'help-echo "Kill this session"
                                   'action (lambda (_)
                                             (ert-flow-kill-session root)
                                             (ert-flow-dashboard)))
               (insert "\n\n")))
           ert-flow--sessions))))
    (display-buffer buf)))

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

(defun ert-flow--schedule-run (sess)
  "Schedule an auto run for SESS with debounce."
  (ert-flow--touch-session sess)
  (when (timerp (ert-flow--session-debounce-timer sess))
    (cancel-timer (ert-flow--session-debounce-timer sess)))
  (let ((delay (ert-flow--conf sess 'debounce-seconds ert-flow-debounce-seconds)))
    (setf (ert-flow--session-debounce-timer sess)
          (run-at-time
           delay
           nil
           (lambda ()
             (ert-flow--touch-session sess)
             (let ((root (ert-flow--session-root sess)))
               (ert-flow--log "debounce: running for %s" root)
               (with-temp-buffer
                 (cd root)
                 (ert-flow-run))))))))

(defun ert-flow--after-save-hook ()
  "Trigger tests after saving a relevant file when watch is enabled (session-aware)."
  (let ((file (buffer-file-name)))
    (when file
      (let* ((root (ert-flow--project-root))
             (sess (ert-flow--get-session root))
             (wmode (ert-flow--conf sess 'watch-mode ert-flow-watch-mode)))
        (when (and (eq wmode 'after-save)
                   (ert-flow--session-watch-enabled sess))
          (let ((ert-flow-watch-include-regexp (ert-flow--conf sess 'watch-include-regexp ert-flow-watch-include-regexp))
                (ert-flow-watch-exclude-regexp (ert-flow--conf sess 'watch-exclude-regexp ert-flow-watch-exclude-regexp)))
            (when (ert-flow--file-event-eligible-p file)
              (ert-flow--log "after-save: scheduling run for %s" file)
              (ert-flow--schedule-run sess))))))))

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
                 (let ((f-dir (file-name-as-directory f)))
                   (unless (and ert-flow-watch-exclude-regexp
                                (string-match-p ert-flow-watch-exclude-regexp f-dir))
                     (walk f (1- depth)))))))))
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

(defun ert-flow--setup-file-notify (sess)
  "Start file-notify watchers for project directories for SESS."
  (let* ((root (ert-flow--session-root sess))
         (handles nil)
         (depth (ert-flow--conf sess 'file-notify-max-depth ert-flow-file-notify-max-depth))
         (inc (ert-flow--conf sess 'watch-include-regexp ert-flow-watch-include-regexp))
         (exc (ert-flow--conf sess 'watch-exclude-regexp ert-flow-watch-exclude-regexp)))
    (ert-flow--touch-session sess)
    (let ((ert-flow-watch-exclude-regexp exc))
      (dolist (dir (ert-flow--collect-dirs root depth))
        (when (file-directory-p dir)
          (let ((h
                 (file-notify-add-watch
                  dir '(change attribute-change)
                  (lambda (event)
                    (condition-case err
                        (pcase event
                          (`(,_ ,action ,path . ,_)
                           (when (memq action '(created changed deleted renamed moved attribute-changed))
                             (let ((ert-flow-watch-include-regexp inc)
                                   (ert-flow-watch-exclude-regexp exc))
                               (when (ert-flow--file-event-eligible-p (ignore-errors (file-truename path)))
                                 (ert-flow--log "file-notify[%s]: %s %s"
                                                (file-name-nondirectory (directory-file-name root))
                                                action path)
                                 (ert-flow--schedule-run sess))))))
                      (error (ert-flow--log "file-notify callback error: %S" err)))))))
            (push h handles)))))
    (setf (ert-flow--session-file-notify-handles sess) (nreverse handles))))

(defun ert-flow--teardown-file-notify (sess)
  "Stop all active file-notify watchers for SESS."
  (dolist (h (ert-flow--session-file-notify-handles sess))
    (ignore-errors (file-notify-rm-watch h)))
  (setf (ert-flow--session-file-notify-handles sess) nil))

(defun ert-flow--enable-watch (sess)
  "Enable the configured watch mode for SESS."
  (ert-flow--touch-session sess)
  (pcase (ert-flow--conf sess 'watch-mode ert-flow-watch-mode)
    ('after-save
     (unless (member #'ert-flow--after-save-hook after-save-hook)
       (add-hook 'after-save-hook #'ert-flow--after-save-hook))
     (cl-incf ert-flow--active-after-save-count))
    ('file-notify
     (ert-flow--setup-file-notify sess))
    (_ nil))
  (ert-flow--ensure-idle-gc-timer))

(defun ert-flow--disable-watch (sess)
  "Disable watch mode for SESS and cancel any pending timers."
  (pcase (ert-flow--conf sess 'watch-mode ert-flow-watch-mode)
    ('after-save
     (when (> ert-flow--active-after-save-count 0)
       (cl-decf ert-flow--active-after-save-count)
       (when (<= ert-flow--active-after-save-count 0)
         (remove-hook 'after-save-hook #'ert-flow--after-save-hook))))
    ('file-notify
     (ert-flow--teardown-file-notify sess)))
  (when (timerp (ert-flow--session-debounce-timer sess))
    (cancel-timer (ert-flow--session-debounce-timer sess)))
  (setf (ert-flow--session-debounce-timer sess) nil)
  (ert-flow--cancel-idle-gc-timer-if-unused))

;;;###autoload
(defun ert-flow-toggle-watch ()
  "Toggle automatic test running (watch) for the current project session."
  (interactive)
  (let* ((root (ert-flow--project-root))
         (sess (ert-flow--get-session root))
         (new (not (ert-flow--session-watch-enabled sess))))
    (setf (ert-flow--session-watch-enabled sess) new)
    (if new
        (progn
          (ert-flow--enable-watch sess)
          (message "ert-flow: watch enabled (%s)" (ert-flow--conf sess 'watch-mode ert-flow-watch-mode)))
      (ert-flow--disable-watch sess)
      (message "ert-flow: watch disabled")))
  (let* ((bufname (ert-flow--session-panel-name (ert-flow--project-root))))
    (when (get-buffer bufname)
      (let ((ert-flow--panel-buffer-name bufname))
        (ert-flow--render)))))

;;;; Copy failures

(defun ert-flow--copy-trim (s limit)
  "Trim string S to LIMIT characters, appending a truncation marker."
  (if (and (integerp limit) (stringp s) (> (length s) limit))
      (concat (substring s 0 limit) "\n... [truncated]")
    (or s "")))

(defun ert-flow--copy-format-header (timestamp n proj runner cmd duration)
  "Return header string for TIMESTAMP, N, PROJ, RUNNER, CMD, DURATION.

CMD and DURATION may be empty strings; they are omitted if empty."
  (let ((meta (concat (format "Project: %s | Runner: %s" proj runner)
                      (if (and (stringp cmd) (> (length cmd) 0))
                          (format " | Command: %s" cmd) "")
                      (if (and (stringp duration) (> (length duration) 0))
                          (format " | Duration: %s" duration) ""))))
    (pcase ert-flow-copy-format
      ('plain    (format "ERT failures (%s): %d\n%s\n" timestamp n meta))
      ('org      (format "* ERT failures (%s): %d\n%s\n" timestamp n meta))
      ('markdown (format "### ERT failures (%s): %d\n%s\n" timestamp n meta))
      (_         (format "ERT failures (%s): %d\n%s\n" timestamp n meta)))))

(defun ert-flow--copy-format-item (r)
  "Return a single list item line for result plist R."
  (let* ((nm (plist-get r :name))
         (msg (or (plist-get r :message) ""))
         (file (plist-get r :file))
         (line (plist-get r :line))
         (loc (cond
               ((and file line) (format " (%s:%s)" file line))
               (file (format " (%s)" file))
               (t ""))))
    (pcase ert-flow-copy-format
      ('plain    (format "- %s%s: %s" nm loc msg))
      ('org      (format "- %s%s :: %s" nm loc msg))
      ('markdown (format "- %s%s: %s" nm loc msg))
      (_         (format "- %s%s: %s" nm loc msg)))))

(defun ert-flow--copy-format-details (name details)
  "Return details block for NAME and DETAILS text."
  (let* ((limit ert-flow-copy-backtrace-limit)
         (txt (ert-flow--copy-trim (or details "") limit)))
    (pcase ert-flow-copy-format
      ('plain    (format "\n=== %s ===\n%s" name txt))
      ('org      (format "\n** %s\n#+begin_example\n%s\n#+end_example\n" name txt))
      ('markdown (format "\n#### %s\n=\n%s\n=\n" name txt))
      (_         (format "\n=== %s ===\n%s" name txt)))))

(defun ert-flow--copy-stdout-tail (raw)
  "Return formatted stdout tail block from RAW or nil."
  (when (and ert-flow-copy-include-stdout (stringp raw) (> (length raw) 0))
    (let* ((limit ert-flow-copy-backtrace-limit)
           (tail (if (and (integerp limit) (> (length raw) limit))
                     (substring raw (- (length raw) limit))
                   raw)))
      (pcase ert-flow-copy-format
        ('plain    (format "\n--- STDOUT tail ---\n%s\n" tail))
        ('org      (format "\n** STDOUT tail\n#+begin_example\n%s\n#+end_example\n" tail))
        ('markdown (format "\n#### STDOUT tail\n=\n%s\n=\n" tail))
        (_         (format "\n--- STDOUT tail ---\n%s\n" tail))))))

(defun ert-flow--copy-stderr-tail (raw)
  "Return formatted stderr tail block from RAW or nil."
  (when (and ert-flow-copy-include-stderr (stringp raw) (> (length raw) 0))
    (let* ((limit ert-flow-copy-backtrace-limit)
           (tail (if (and (integerp limit) (> (length raw) limit))
                     (substring raw (- (length raw) limit))
                   raw)))
      (pcase ert-flow-copy-format
        ('plain    (format "\n--- STDERR tail ---\n%s\n" tail))
        ('org      (format "\n** STDERR tail\n#+begin_example\n%s\n#+end_example\n" tail))
        ('markdown (format "\n#### STDERR tail\n=\n%s\n=\n" tail))
        (_         (format "\n--- STDERR tail ---\n%s\n" tail))))))

;;;###autoload
(defun ert-flow-copy-failures ()
  "Copy failures/errors from the last run into the kill-ring (with backtraces).

Respects:
- `ert-flow-copy-format' — 'plain (default) | 'org | 'markdown
- `ert-flow-copy-backtrace-limit' — truncate details if set
- `ert-flow-copy-include-stdout' — include raw stdout tail"
  (interactive)
  (let* ((root (ert-flow--project-root))
         (sess (ert-flow--get-session root))
         (results (or (and sess (ert-flow--session-last-results sess)) ert-flow--last-results))
         (sum (or (and sess (ert-flow--session-last-summary sess)) ert-flow--last-summary))
         (raw (or (and sess (ert-flow--session-last-raw-output sess)) ert-flow--last-raw-output))
         (fails (seq-filter
                 (lambda (r) (memq (plist-get r :status) '(fail error)))
                 (or results '())))
         (ts (format-time-string "%Y-%m-%d %H:%M:%S"))
         (proj (file-name-nondirectory (directory-file-name root)))
         (runner-sym (ert-flow--conf sess 'runner ert-flow-runner))
         (runner (if (eq runner-sym 'in-emacs-ert) "in-emacs" "external"))
         (ext (ert-flow--conf sess 'external-command ert-flow-external-command))
         (cmd-str (cond
                   ((and (eq runner-sym 'external-command) (listp ext)) (mapconcat #'identity ext " "))
                   ((and (eq runner-sym 'external-command) (stringp ext)) ext)
                   (t "")))
         (dur-ms (and (listp sum) (alist-get 'duration-ms sum)))
         (dur-str (cond ((numberp dur-ms) (format "%.3fs" (/ dur-ms 1000.0))) (t ""))))
    (if (null fails)
        (progn
          (kill-new (format "ERT failures (%s): 0\nProject: %s | Runner: %s%s%s\nNo failures."
                            ts proj runner
                            (if (> (length cmd-str) 0) (format " | Command: %s" cmd-str) "")
                            (if (> (length dur-str) 0) (format " | Duration: %s" dur-str) "")))
          (message "ert-flow: no failures"))
      (let* ((header (ert-flow--copy-format-header ts (length fails) proj runner cmd-str dur-str))
             (body (mapconcat #'ert-flow--copy-format-item fails "\n"))
             (details (mapconcat
                       (lambda (r)
                         (ert-flow--copy-format-details (plist-get r :name)
                                                        (plist-get r :details)))
                       fails ""))
             (stdout-block (ert-flow--copy-stdout-tail raw))
             (stderr-raw (and sess (ert-flow--session-last-stderr-output sess)))
             (stderr-block (ert-flow--copy-stderr-tail stderr-raw)))
        (kill-new (concat header body "\n" details (or stdout-block "") (or stderr-block "")))
        (message "ert-flow: failures copied")))))

;;;###autoload
(defun ert-flow-clear ()
  "Clear panel and last results for the current session."
  (interactive)
  (let* ((sess (ert-flow--get-session (ert-flow--project-root))))
    (when sess
      (setf (ert-flow--session-last-summary sess) nil
            (ert-flow--session-last-results sess) nil
            (ert-flow--session-last-raw-output sess) nil)))
  ;; keep legacy globals in sync
  (setq ert-flow--last-summary nil
        ert-flow--last-results nil
        ert-flow--last-raw-output nil)
  (let* ((bufname (ert-flow--session-panel-name (ert-flow--project-root))))
    (when (get-buffer bufname)
      (let ((ert-flow--panel-buffer-name bufname))
        (ert-flow--render))))
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
