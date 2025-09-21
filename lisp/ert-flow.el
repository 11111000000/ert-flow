;;; ert-flow.el --- Minimal ERT flow: run external tests, parse, show panel -*- lexical-binding: t; -*-
;; Author: Peter Kosov <11111000000@email.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: tools, lisp, testing
;; URL: https://github.com/11111000000/ert-flow

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
(require 'gv)
(require 'ert)
(require 'seq)
(require 'project)
(require 'json)
(require 'filenotify)
(require 'button)
(require 'all-the-icons nil t)
(require 'ert-flow-headerline nil t)
(require 'ert-flow-view-controls nil t)
(require 'ert-flow-controls-icons nil t)

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
  '((t :foreground "SpringGreen3" :weight bold))
  "Face for passed test icons."
  :group 'ert-flow)

(defface ert-flow-face-fail
  '((t :foreground "red3" :weight bold))
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

(defcustom ert-flow-auto-detect-on-open t
  "If non-nil, auto-detect external command when opening the panel.

When the runner is 'external-command and a session has no command configured,
`ert-flow-open-panel' will look for common entrypoints like tests/run-tests.el
(and test/run-tests.el) and set a per-session command automatically."
  :type 'boolean
  :group 'ert-flow)

(defcustom ert-flow-run-on-open t
  "If non-nil, run tests once upon opening the panel when no results are present.

This first-run happens only once per session per Emacs session and respects the
selected runner:
- external-command: runs if a command is configured or auto-detected
- in-emacs-ert: runs unconditionally"
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
  "Queue (FIFO) of pending runs as items: plist with :thunk :label :root :cmd.")

(defvar ert-flow--idle-gc-timer nil
  "Global timer that periodically auto-disables idle session watchers.")

(defun ert-flow--log (fmt &rest args)
  "Log a debug message FMT with ARGS when `ert-flow-log-enabled' is non-nil."
  (when ert-flow-log-enabled
    (apply #'message (concat "[ert-flow] " fmt) args)))

(defun ert-flow--dbg-sess (s)
  "Return a concise debug string describing session S."
  (when s
    (format "root=%s panel=%s details=%s watch=%s runner=%s parser=%s last-parser=%s proc=%s"
            (ert-flow--session-root s)
            (ert-flow--session-panel-buf-name s)
            (ert-flow--session-details-buf-name s)
            (if (ert-flow--get-watch-enabled s) "On" "Off")
            (ert-flow--conf s 'runner ert-flow-runner)
            (ert-flow--conf s 'parser ert-flow-parser)
            (or (ert-flow--get-last-parser s) "-")
            (if (process-live-p (ert-flow--session-process s)) "live" "nil"))))

(defun ert-flow--dbg-conf (s)
  "Return a concise debug string describing important config of session S."
  (when s
    (format "cfg: side=%s width=%s debounce=%s include=%S exclude=%S ext-cmd=%S"
            (ert-flow--conf s 'panel-side ert-flow-panel-side)
            (ert-flow--conf s 'panel-width ert-flow-panel-width)
            (ert-flow--conf s 'debounce-seconds ert-flow-debounce-seconds)
            (ert-flow--conf s 'watch-include-regexp ert-flow-watch-include-regexp)
            (ert-flow--conf s 'watch-exclude-regexp ert-flow-watch-exclude-regexp)
            (ert-flow--conf s 'external-command ert-flow-external-command))))

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
  "Return icon string for STATUS, using Material icons if available."
  (let ((icons-ok (and (memq ert-flow-toolbar-style '(auto icons))
                       (featurep 'all-the-icons)
                       (display-graphic-p)
                       (find-font (font-spec :family "Material Icons"))
                       (fboundp 'all-the-icons-material))))
    (if icons-ok
        (pcase status
          ('pass  (all-the-icons-material "check_circle"))
          ('fail  (all-the-icons-material "cancel"))
          ('error (all-the-icons-material "error"))
          ('skip  (all-the-icons-material "remove_circle_outline"))
          ('xfail (all-the-icons-material "remove_circle_outline"))
          (_      (all-the-icons-material "help")))
      (or (cdr (assq status ert-flow--status-icons)) "?"))))

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

(defun ert-flow--batch-clean-name (s)
  "Return NAME from ERT summary line, stripping timing/location suffixes.
Examples:
  \"ert-flow/render-smoke (0.000 sec) at tests/foo.el:12\" -> \"ert-flow/render-smoke\"."
  (let* ((nm (ert-flow--string-trim s)))
    (if (string-match "\\`\\([^ ]+\\)" nm)
        (match-string 1 nm)
      nm)))

(defun ert-flow--batch--extract-details-block (lines i)
  "Extract details block starting after index I in LINES.
Return cons (block . next-i) where next-i is position to continue loop."
  (let* ((start (1+ i))
         (j start))
    (while (and (< j (length lines))
                (not (string-match "^Test[ \t]+" (nth j lines))))
      (cl-incf j))
    (cons (string-join (cl-subseq lines start j) "\n") (1- j))))

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
     ;; Style A: "Test NAME passed."
     (when (string-match "^Test[ \t]+\\([^ \t]+\\)[ \t]+passed\\.$" line)
       (let ((nm (match-string 1 line)))
         (puthash nm t all-names)
         (puthash nm 'pass name->status)))
     ;; Style B: progress lines: "passed 1/15 NAME"
     (when (string-match "^[ \t]*passed[ \t]+[0-9]+/[0-9]+[ \t]+\\([^ \t]+\\)" line)
       (let ((nm (match-string 1 line)))
         (puthash nm t all-names)
         (puthash nm 'pass name->status)))
     ;; Early classification
     (when (string-match "^[ \t]*\\(FAILED\\|ERROR\\|SKIPPED\\|XFAIL\\|XPASS\\)[ \t]+[0-9]+/[0-9]+[ \t]+\\(.+\\)$" line)
       (let* ((kw (match-string 1 line))
              (nm (ert-flow--batch-clean-name (match-string 2 line)))
              (st (pcase kw
                    ("FAILED"  'fail)
                    ("ERROR"   'error)
                    ("SKIPPED" 'skip)
                    ("XFAIL"   'xfail)
                    ("XPASS"   'fail)
                    (_ 'fail))))
         (puthash nm t all-names)
         (puthash nm st name->status)))
     ;; Details blocks
     (when (string-match "^Test[ \t]+\\([^ \t]+\\)[ \t]+\\(backtrace\\|condition\\):$" line)
       (let* ((nm (match-string 1 line))
              (pair (ert-flow--batch--extract-details-block lines i))
              (block (car pair))
              (next-i (cdr pair)))
         (puthash nm t all-names)
         (puthash nm (ert-flow--string-trim block) name->details)
         (setq i next-i)))
     ;; Totals and time
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
- XPASS  → fail  (unexpected pass is treated as failure)
Also tolerates progress counters like \"FAILED 2/15 NAME\"."
  (dolist (line lines)
    (when (string-match "^[ \t]*\\(FAILED\\|ERROR\\|SKIPPED\\|XFAIL\\|XPASS\\)\\(?:[ \t]+[0-9]+/[0-9]+\\)?[ \t]+\\(.+\\)$" line)
      (let* ((kw (match-string 1 line))
             (nm (ert-flow--batch-clean-name (match-string 2 line)))
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
    (ert-flow--log "batch parsed: total=%s results=%d time=%s"
                   (or (alist-get 'total summary) "?")
                   (length results)
                   (or (alist-get 'time summary) "-"))
    (when (and (= (length results) 0)
               (stringp out) (> (length out) 0))
      (ert-flow--log "batch parsed 0 results; first 200 chars:\n%s"
                     (substring out 0 (min 200 (length out)))))
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

(defun ert-flow--aget (key obj)
  "Lookup KEY in alist OBJ, tolerating string or symbol keys.

If KEY is a string, also tries its interned symbol.
If KEY is a symbol, also tries its name as a string."
  (or (alist-get key obj nil nil #'equal)
      (and (stringp key) (alist-get (intern key) obj))
      (and (symbolp key) (alist-get (symbol-name key) obj nil nil #'equal))))

(defun ert-flow--json-parse (out)
  "Return parsed JSON object (alist) from OUT or signal on failure."
  (let* ((json-str (or (ert-flow--json-safe-substring out) out)))
    (json-parse-string json-str :object-type 'alist :array-type 'list)))

(defun ert-flow--json-tests (obj)
  "Extract tests array from OBJ, returning a list or nil."
  (let ((tests-raw (ert-flow--aget "tests" obj)))
    (cond
     ((null tests-raw) nil)
     ((listp tests-raw) tests-raw)
     ((vectorp tests-raw) (append tests-raw nil))
     (t (signal 'wrong-type-argument (list 'list tests-raw))))))

(defun ert-flow--json-test->plist (tobj)
  "Convert a single TOBJ (alist from JSON) into an ert-flow result plist."
  (let* ((nm (ert-flow--aget "name" tobj))
         (st-str (ert-flow--aget "status" tobj))
         (st (pcase (and st-str (downcase (format "%s" st-str)))
               ("pass" 'pass) ("ok" 'pass)
               ("fail" 'fail) ("failed" 'fail)
               ("error" 'error)
               ("skip" 'skip) ("skipped" 'skip)
               ("xfail" 'xfail)
               (_ 'fail)))
         (msg (ert-flow--aget "message" tobj))
         (det (ert-flow--aget "details" tobj))
         (file (ert-flow--aget "file" tobj))
         (line (ert-flow--aget "line" tobj))
         (suite (ert-flow--suite-of nm)))
    (list :name (and nm (format "%s" nm))
          :status st
          :message (or msg (and det (car (split-string (format "%s" det) "\n" t))) (symbol-name st))
          :details (or (and det (format "%s" det)) "")
          :suite suite
          :file (and file (format "%s" file))
          :line (and line (string-to-number (format "%s" line))))))

(defun ert-flow--json-build-summary (summary-raw results)
  "Build summary alist from SUMMARY-RAW (alist) and RESULTS (list)."
  (let* ((raw-total (and summary-raw (ert-flow--aget "total" summary-raw)))
         (total (cond
                 ((numberp raw-total) raw-total)
                 ((and (stringp raw-total) (string-match-p "\\`[0-9]+\\'" raw-total))
                  (string-to-number raw-total))
                 (t (length results))))
         (p (or (and summary-raw (ert-flow--aget "passed" summary-raw))
                (cl-count-if (lambda (r) (eq (plist-get r :status) 'pass)) results)))
         (f (or (and summary-raw (ert-flow--aget "failed" summary-raw))
                (cl-count-if (lambda (r) (eq (plist-get r :status) 'fail)) results)))
         (e (or (and summary-raw (ert-flow--aget "error" summary-raw))
                (cl-count-if (lambda (r) (eq (plist-get r :status) 'error)) results)))
         (s (or (and summary-raw (ert-flow--aget "skipped" summary-raw))
                (cl-count-if (lambda (r) (memq (plist-get r :status) '(skip xfail))) results)))
         (unexpected (seq-count (lambda (r) (memq (plist-get r :status) '(fail error))) results))
         (duration-ms
          (let ((dm (and summary-raw (ert-flow--aget "duration_ms" summary-raw)))
                (tstr (and summary-raw (ert-flow--aget "time" summary-raw))))
            (cond
             ((numberp dm) dm)
             ((and (stringp dm) (string-match-p "\\`[0-9]+\\'" dm)) (string-to-number dm))
             ((and (stringp tstr) (string-match-p "\\`[0-9.]+\\'" tstr))
              (truncate (* 1000 (string-to-number tstr))))
             (t nil))))
         (time-str (let ((tval (and summary-raw (ert-flow--aget "time" summary-raw))))
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
             (summary-raw (ert-flow--aget "summary" obj))
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
    (ert-flow--set-last-activity-at sess (current-time))))

(defun ert-flow--any-watch-enabled-p ()
  "Return non-nil if any session has watch enabled."
  (let (found)
    (maphash (lambda (_ s)
               (when (and (not found) (ert-flow--get-watch-enabled s))
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
           (let ((last (ert-flow--get-last-activity-at s)))
             (when (and (ert-flow--get-watch-enabled s) last)
               (let* ((idle (float-time (time-subtract now last))))
                 (when (and (numberp idle)
                            (> idle ert-flow-session-idle-seconds))
                   (ert-flow--set-watch-enabled s nil)
                   (ert-flow--disable-watch s)
                   (ert-flow--log "idle-gc: disabled watch for %s after %.1fs"
                                  (ert-flow--session-root s) idle))))))
         ert-flow--sessions)
        (ert-flow--cancel-idle-gc-timer-if-unused))
    (error (ert-flow--log "idle-gc error: %S" err))))

(defun ert-flow--enqueue-run (label root thunk &optional cmd)
  "Enqueue THUNK with LABEL and ROOT (and optional CMD) for later execution."
  (let ((item (list :thunk thunk :label label :root root :cmd cmd)))
    (setq ert-flow--run-queue (append ert-flow--run-queue (list item)))))

(defun ert-flow--dequeue-run ()
  "Dequeue and return next run item plist or nil."
  (let ((head (car ert-flow--run-queue)))
    (setq ert-flow--run-queue (cdr ert-flow--run-queue))
    head))

(defun ert-flow--log-concurrency-state ()
  "Log snapshot of concurrency: active processes and queued items."
  (when ert-flow-log-enabled
    (let (actives)
      (maphash
       (lambda (_ s)
         (let ((p (ert-flow--session-process s)))
           (when (process-live-p p)
             (push (format "%s label=%s pid=%s cmd=%S"
                           (ert-flow--session-root s)
                           (or (process-get p 'ert-flow-label) "-")
                           (ignore-errors (process-id p))
                           (process-get p 'ert-flow-cmd))
                   actives))))
       ert-flow--sessions)
      (ert-flow--log "state: active=%d queued=%d" ert-flow--active-run-count (length ert-flow--run-queue))
      (when actives
        (dolist (ln (nreverse actives))
          (ert-flow--log "active: %s" ln)))
      (when ert-flow--run-queue
        (let ((i 0))
          (dolist (item ert-flow--run-queue)
            (setq i (1+ i))
            (ert-flow--log "queued[%d]: root=%s label=%s cmd=%S"
                           i (plist-get item :root) (plist-get item :label) (plist-get item :cmd))))))))

;;;###autoload
(defun ert-flow-dump-concurrency ()
  "Dump current concurrency state to *Messages*."
  (interactive)
  (let ((ert-flow-log-enabled t))
    (ert-flow--log-concurrency-state)))

(defun ert-flow--maybe-start-run (sess cmd label)
  "Start CMD for SESS with LABEL respecting concurrency limit."
  (let ((root (ert-flow--session-root sess)))
    (if (>= ert-flow--active-run-count ert-flow-max-concurrent-runs)
        (progn
          (ert-flow--log "queue: %s (limit %d reached) active=%d queued=%d root=%s"
                         label ert-flow-max-concurrent-runs
                         ert-flow--active-run-count
                         (length ert-flow--run-queue)
                         root)
          (ert-flow--enqueue-run label root (lambda () (ert-flow--start-run-internal sess cmd label)) cmd)
          (ert-flow-open-panel)
          (with-current-buffer (get-buffer-create (ert-flow--session-panel-name root))
            (let ((inhibit-read-only t))
              (goto-char (point-max))
              (insert (propertize (format "Queued: %s\n" label) 'face 'shadow))))
          (ert-flow--log-concurrency-state))
      (cl-incf ert-flow--active-run-count)
      (ert-flow--log "start: %s active=%d→%d queued=%d root=%s"
                     label (1- ert-flow--active-run-count) ert-flow--active-run-count
                     (length ert-flow--run-queue) root)
      (ert-flow--start-run-internal sess cmd label)
      (ert-flow--log-concurrency-state))))

(defun ert-flow--start-run-internal (sess cmd label)
  "Actually start the external process CMD for SESS, annotating LABEL."
  (ert-flow--touch-session sess)
  (let* ((root (ert-flow--session-root sess))
         (panel (ert-flow--session-panel-name root)))
    (when (process-live-p (ert-flow--session-process sess))
      (ert-flow--log "Killing previous process...")
      (ignore-errors (kill-process (ert-flow--session-process sess))))
    (ert-flow--set-last-raw-output sess nil)
    (ert-flow--set-last-stderr-output sess nil)
    (ert-flow-open-panel)
    (with-current-buffer (get-buffer-create panel)
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert (propertize (format "Running: %s %S\n" label cmd) 'face 'shadow))))
    (let ((default-directory root))
      (ert-flow--log "spawn: root=%s cmd=%S label=%s" root cmd label)
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
        (process-put p 'ert-flow-label label)
        (process-put p 'ert-flow-root root)
        (process-put p 'ert-flow-cmd cmd)
        (ert-flow--set-process sess p)
        ;; legacy global for compatibility
        (setq ert-flow--process p)
        (ert-flow--log-concurrency-state)))))

;;;; Internal run finish and in-Emacs runner

(defun ert-flow--finish-run ()
  "Bookkeeping after a run finishes: free a slot and start next queued run."
  (when (> ert-flow--active-run-count 0)
    (cl-decf ert-flow--active-run-count))
  (ert-flow--log "finish: freed slot → active=%d queued=%d" ert-flow--active-run-count (length ert-flow--run-queue))
  (let ((next (ert-flow--dequeue-run)))
    (when next
      (cl-incf ert-flow--active-run-count)
      (ert-flow--log "dequeue: start %s active=%d queued=%d root=%s"
                     (or (plist-get next :label) "?")
                     ert-flow--active-run-count
                     (length ert-flow--run-queue)
                     (or (plist-get next :root) "?"))
      (funcall (plist-get next :thunk))))
  (ert-flow--log-concurrency-state))

(defun ert-flow--maybe-start-thunk (sess label thunk)
  "Start THUNK respecting concurrency for SESS and LABEL."
  (let ((root (ert-flow--session-root sess)))
    (if (>= ert-flow--active-run-count ert-flow-max-concurrent-runs)
        (progn
          (ert-flow--log "queue(thunk): %s (limit %d reached) active=%d queued=%d root=%s"
                         label ert-flow-max-concurrent-runs
                         ert-flow--active-run-count
                         (length ert-flow--run-queue) root)
          (ert-flow--enqueue-run label root thunk nil)
          (ert-flow-open-panel)
          (with-current-buffer (get-buffer-create (ert-flow--session-panel-name root))
            (let ((inhibit-read-only t))
              (goto-char (point-max))
              (insert (propertize (format "Queued: %s\n" label) 'face 'shadow))))
          (ert-flow--log-concurrency-state))
      (cl-incf ert-flow--active-run-count)
      (ert-flow--log "start(thunk): %s active=%d→%d queued=%d root=%s"
                     label (1- ert-flow--active-run-count) ert-flow--active-run-count
                     (length ert-flow--run-queue) root)
      (funcall thunk)
      (ert-flow--log-concurrency-state))))

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

(defun ert-flow--pp-backtrace (bt)
  "Pretty-print ERT backtrace object BT into a readable multi-line string.

Accepts:
- string (returned as-is)
- list/vector of frames (printed one per line as \"#<n> <frame>\").
Falls back to `format' with %S if the structure is unknown."
  (cond
   ((null bt) "")
   ((stringp bt) (string-trim-right bt))
   ((or (listp bt) (vectorp bt))
    (let* ((seq (if (vectorp bt) (append bt nil) bt))
           (i -1))
      (string-join
       (mapcar (lambda (frame)
                 (cl-incf i)
                 (format "  #%d %s" i
                         (condition-case _
                             (cond
                              ;; Try to unpack common frame shapes a bit; otherwise print raw
                              ((and (consp frame)
                                    (symbolp (car frame)))
                               (format "%S" frame))
                              (t (format "%S" frame)))
                           (error (format "%S" frame)))))
               seq)
       "\n")))
   (t (format "%S" bt))))

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
            (setq details
                  (concat details
                          "Backtrace:\n"
                          (ert-flow--pp-backtrace bt)
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
        (ert-flow--set-last-summary sess summary)
        (ert-flow--set-last-results sess results)
        (ert-flow--set-last-raw-output sess nil)
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
    (define-key map (kbd "TAB") #'ert-flow-tab)
    (define-key map (kbd "<backtab>") #'ert-flow-toggle-all-groups)
    ;; Navigation
    (define-key map (kbd "n") #'ert-flow-next-item)
    (define-key map (kbd "p") #'ert-flow-previous-item)
    ;; Additional vim-like navigation
    (define-key map (kbd "j") #'ert-flow-next-item)
    (define-key map (kbd "k") #'ert-flow-previous-item)
    ;; Panel filters
    (define-key map (kbd "P") #'ert-flow-panel-filter-pass)
    (define-key map (kbd "F") #'ert-flow-panel-filter-fail)
    (define-key map (kbd "E") #'ert-flow-panel-filter-error)
    (define-key map (kbd "S") #'ert-flow-panel-filter-skip)
    (define-key map (kbd "A") #'ert-flow-panel-filter-all)
    (define-key map (kbd "/") #'ert-flow-panel-set-name-filter)
    (define-key map (kbd "T") #'ert-flow-panel-set-tags-filter)
    (define-key map (kbd "C") #'ert-flow-panel-filter-clear)
    map)
  "Keymap for `ert-flow-panel-mode'.")

(define-derived-mode ert-flow-panel-mode special-mode "ert-flow_panel"
  "Major mode for displaying ERT results in a side panel."
  (setq buffer-read-only t
        truncate-lines t)
  ;; Buffer-local fold state for suite groups
  (setq-local ert-flow--folded-suites (or ert-flow--folded-suites
                                          (make-hash-table :test 'equal)))
  ;; Ensure header-line module is loaded, then apply controls if enabled.
  (ignore-errors (require 'ert-flow-headerline nil t))
  (when (and (boundp 'ert-flow-view-headerline-enable)
             ert-flow-view-headerline-enable
             (fboundp 'ert-flow-headerline--apply))
    (ert-flow-headerline--apply (current-buffer)))
  (when (fboundp 'ert-flow-view-controls--ensure-headerline-face)
    (ignore-errors (ert-flow-view-controls--ensure-headerline-face))))

;; Override button TAB behavior: fold/unfold instead of jumping
(defvar ert-flow--suite-button-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map button-map)
    (define-key map (kbd "TAB") #'ert-flow-toggle-group-at-point)
    (define-key map [tab] #'ert-flow-toggle-group-at-point)
    (define-key map (kbd "<tab>") #'ert-flow-toggle-group-at-point)
    (define-key map (kbd "<backtab>") #'ert-flow-toggle-all-groups)
    ;; Allow navigation keys to work even when point is on a button
    (define-key map (kbd "n") #'ert-flow-next-item)
    (define-key map (kbd "p") #'ert-flow-previous-item)
    (define-key map (kbd "j") #'ert-flow-next-item)
    (define-key map (kbd "k") #'ert-flow-previous-item)
    map)
  "Keymap for suite heading buttons that makes TAB fold/unfold instead of moving.")

(defvar ert-flow--status-button-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map button-map)
    ;; TAB toggles Status fold/unfold
    (define-key map (kbd "TAB") #'ert-flow-toggle-status)
    (define-key map [tab] #'ert-flow-toggle-status)
    (define-key map (kbd "<tab>") #'ert-flow-toggle-status)
    ;; Navigation passthrough
    (define-key map (kbd "n") #'ert-flow-next-item)
    (define-key map (kbd "p") #'ert-flow-previous-item)
    (define-key map (kbd "j") #'ert-flow-next-item)
    (define-key map (kbd "k") #'ert-flow-previous-item)
    map)
  "Keymap for Status heading (TAB folds/unfolds, mouse-1 toggles).")

(defvar ert-flow--toolbar-button-map
  (let ((map (make-sparse-keymap())))
    (set-keymap-parent map button-map)
    (define-key map (kbd "TAB") #'ert-flow-toggle-group-at-point)
    (define-key map [tab] #'ert-flow-toggle-group-at-point)
    (define-key map (kbd "<tab>") #'ert-flow-toggle-group-at-point)
    (define-key map (kbd "<backtab>") #'ert-flow-toggle-all-groups)
    ;; Allow navigation keys to work even when point is on a button
    (define-key map (kbd "n") #'ert-flow-next-item)
    (define-key map (kbd "p") #'ert-flow-previous-item)
    (define-key map (kbd "j") #'ert-flow-next-item)
    (define-key map (kbd "k") #'ert-flow-previous-item)
    map)
  "Keymap for toolbar buttons that makes TAB fold/unfold the nearest group.")

(defun ert-flow--open-panel--maybe-autodetect (sess root runner ext-cmd)
  "Return (ext-cmd . auto-did-detect) after optional auto-detect for SESS at ROOT."
  (let* ((auto-did-detect nil)
         (root* (file-name-as-directory root))
         (local-run-tests
          (lambda ()
            (let* ((cand1 (expand-file-name "tests/run-tests.el" root*))
                   (cand2 (expand-file-name "test/run-tests.el" root*)))
              (cond
               ((file-exists-p cand1) cand1)
               ((file-exists-p cand2) cand2)
               (t nil)))))
         ;; Try to locate -l PATH in list-form command; ignore shell-string case.
         (list-cmd-path (when (and (listp ext-cmd) (member "-l" ext-cmd))
                          (let ((idx (cl-position "-l" ext-cmd :test #'string=)))
                            (and idx (nth (1+ idx) ext-cmd)))))
         ;; ext-cmd is considered "alien" if it points outside of current ROOT.
         (mismatch (and (eq runner 'external-command)
                        ext-cmd
                        (stringp list-cmd-path)
                        (file-name-absolute-p list-cmd-path)
                        (not (string-prefix-p root* (file-name-directory list-cmd-path))))))
    (when (and ert-flow-auto-detect-on-open
               (eq runner 'external-command)
               (or (not ext-cmd) mismatch))
      (let ((found (funcall local-run-tests)))
        (when found
          (ert-flow--set-conf sess 'external-command (list "emacs" "-Q" "--batch" "-l" found))
          (setq ext-cmd (ert-flow--conf sess 'external-command ert-flow-external-command))
          (setq auto-did-detect t)
          (ert-flow--log (if mismatch
                             "auto-detect: replaced alien external cmd with %s"
                           "auto-detect: set external cmd to %s")
                         (if (string-match-p "/test[s]?/run-tests\\.el\\'" found)
                             (file-relative-name found root*)
                           found)))))
    (cons ext-cmd auto-did-detect)))

(defun ert-flow--open-panel--display (sess root)
  "Display panel window for SESS at ROOT and render."
  (let* ((bufname (ert-flow--session-panel-name root))
         (buf (get-buffer-create bufname))
         (win (display-buffer-in-side-window
               buf
               `((side . ,(ert-flow--conf sess 'panel-side ert-flow-panel-side))
                 (window-width . ,(ert-flow--conf sess 'panel-width ert-flow-panel-width))))))
    (ert-flow--touch-session sess)
    (with-current-buffer buf
      ;; Ensure the panel buffer is rooted at the project for reliable fallbacks
      (setq default-directory (file-name-as-directory root))
      (ert-flow-panel-mode)
      (let ((ert-flow--panel-buffer-name bufname))
        (ert-flow--render)))
    (select-window win)))

(defun ert-flow--open-panel--maybe-first-run (sess runner ext-cmd auto-did-detect)
  "Maybe trigger first run for SESS based on RUNNER, EXT-CMD and AUTO-DID-DETECT."
  (let* ((root (ert-flow--session-root sess))
         (norm (and ext-cmd (ert-flow--normalize-command ext-cmd))))
    (when (and ert-flow-run-on-open
               (not (ert-flow--conf sess 'first-open-run-done nil))
               (null (ert-flow--session-last-results sess)))
      (cond
       ((eq runner 'in-emacs-ert)
        (ert-flow--set-conf sess 'first-open-run-done t)
        (ert-flow--log "first-open-run: in-emacs (root=%s)" root)
        (ert-flow-run))
       ((and (eq runner 'external-command) norm)
        (ert-flow--set-conf sess 'first-open-run-done t)
        (ert-flow--log "first-open-run: external (root=%s cmd=%S)" root norm)
        (ert-flow-run))
       (auto-did-detect
        (ert-flow--set-conf sess 'first-open-run-done t)
        (ert-flow--log "first-open-run: external (auto-detected) (root=%s)" root)
        (ert-flow-run))
       (t
        (ert-flow--log "first-open-run: skipped (root=%s) reason=%s"
                       root
                       (cond
                        ((and (eq runner 'external-command) (not norm)) "no external command")
                        (t "unknown"))))))))

;;;###autoload
(defun ert-flow-open-panel ()
  "Open or focus the ert-flow panel (session-aware)."
  (interactive)
  (let* ((root (ert-flow--project-root))
         (sess (ert-flow--get-session root))
         (runner (ert-flow--conf sess 'runner ert-flow-runner))
         (ext-cmd (ert-flow--conf sess 'external-command ert-flow-external-command)))
    (pcase-let ((`(,ext-cmd* . ,auto-did) (ert-flow--open-panel--maybe-autodetect sess root runner ext-cmd)))
      (ert-flow--log "open-panel: root=%s name=%s runner=%s parser=%s log=%s ext=%S auto-detected=%s panel=%s | %s | %s"
                     root
                     (funcall ert-flow-session-naming-function root)
                     runner
                     (ert-flow--conf sess 'parser ert-flow-parser)
                     (if ert-flow-log-enabled "on" "off")
                     ext-cmd*
                     auto-did
                     (ert-flow--session-panel-name root)
                     (ert-flow--dbg-sess sess)
                     (ert-flow--dbg-conf sess))
      (ert-flow--open-panel--display sess root)
      (ert-flow--open-panel--maybe-first-run sess runner ext-cmd* auto-did))))

(defun ert-flow--propertize (text result)
  "Return TEXT with RESULT stored as a text property."
  (propertize text 'ert-flow--result result))

(defun ert-flow--toolbar-icon (key face &optional _alt)
  "Return a compact icon string for toolbar KEY using FACE.

This implementation uses simple Unicode/emoji so it works everywhere
without external fonts. Known KEYs: run, run-failed, watch, watch-on,
watch-off, copy, clear, detect, goto."
  (let ((glyph
         (pcase key
           ('run        "▶")
           ('run-failed "↻")
           ('watch      "W")
           ('watch-on   "O")
           ('watch-off  "🙈")
           ('copy       "📋")
           ('clear      "C")
           ('detect     "🔎")
           ('goto       "↗")
           (_           "•"))))
    (propertize glyph 'face face)))

(defun ert-flow--insert-toolbar-button (key face cmd icon-alt help)
  "Insert a single toolbar button with KEY, FACE, CMD, ICON-ALT and HELP."
  (let* ((icon (ert-flow--toolbar-icon key face icon-alt))
         (text (format " %s " icon)))
    (insert-text-button
     text
     'face face
     'mouse-face 'highlight
     'help-echo help
     'follow-link t
     'keymap ert-flow--toolbar-button-map
     'action (lambda (_btn) (call-interactively cmd)))))

(defun ert-flow--run-failed-help (runner ext-cmd failed-fn)
  "Return help text for the Run Failed button given RUNNER, EXT-CMD and FAILED-FN."
  (if (eq runner 'in-emacs-ert)
      "Run failed tests in-Emacs (ERT selector) (f)"
    (if (and (listp ext-cmd) failed-fn)
        "Run failed tests (external argv via failed-args) (f)"
      "Run failed tests (falls back to all) (f)")))

(defun ert-flow--watch-icon (on)
  "Return watch icon based on ON."
  (if on "🙈" "👁"))

;; Toolbar is now obsolete (panel controls in header-line), this is a no-op.
(defun ert-flow--insert-toolbar (_sess) ())

(defun ert-flow--find-panel-session ()
  "Return session object for the current panel buffer.

Prefer the actual buffer name (panel buffer) to resolve the session.
Do not rely on global `ert-flow--panel-buffer-name' outside render-time."
  (let* ((panel-name
          (or (and (eq major-mode 'ert-flow-panel-mode) (buffer-name))
              (and (stringp ert-flow--panel-buffer-name) ert-flow--panel-buffer-name)
              (buffer-name)))
         (found nil))
    (maphash
     (lambda (_root s)
       (when (and (not found)
                  (string= (ert-flow--session-panel-buf-name s) panel-name))
         (setq found s)))
     ert-flow--sessions)
    (if found
        (ert-flow--log "find-panel-session: panel=%s -> root=%s" panel-name (ert-flow--session-root found))
      (ert-flow--log "find-panel-session: panel=%s not found; fallback to root=%s"
                     panel-name (ert-flow--project-root)))
    (or found (ert-flow--get-session (ert-flow--project-root)))))

(defun ert-flow--insert-header_line (_sess)
  "Header-line hosts controls; nothing to insert here now."
  (ignore _sess))

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

;; summary is now in status-block, do nothing
(defun ert-flow--insert-summary-line (_sum _results) ())

(defun ert-flow--apply-panel-filters (results)
  "Apply panel-local filters to RESULTS and return a filtered list.

Respects:
- `ert-flow--panel-status-filter' — list of allowed statuses or nil (all)
- `ert-flow--panel-name-regexp'   — regexp for name match or nil (no filter)
- `ert-flow--panel-tags-filter'   — list of tags (strings); test passes if any tag matches"
  (let ((rs (or results '())))
    ;; Only apply filters inside an ert-flow panel buffer
    (if (not (eq major-mode 'ert-flow-panel-mode))
        rs
      ;; status filter
      (when (and (boundp 'ert-flow--panel-status-filter)
                 ert-flow--panel-status-filter)
        (setq rs (seq-filter
                  (lambda (r) (memq (plist-get r :status) ert-flow--panel-status-filter))
                  rs)))
      ;; name filter
      (when (and (boundp 'ert-flow--panel-name-regexp)
                 (stringp ert-flow--panel-name-regexp)
                 (> (length ert-flow--panel-name-regexp) 0))
        (setq rs (seq-filter
                  (lambda (r)
                    (let ((nm (or (plist-get r :name) "")))
                      (ignore-errors (string-match-p ert-flow--panel-name-regexp nm))))
                  rs)))
      ;; tags filter
      (when (and (boundp 'ert-flow--panel-tags-filter)
                 (listp ert-flow--panel-tags-filter)
                 ert-flow--panel-tags-filter)
        (setq rs (seq-filter
                  (lambda (r)
                    (let ((tags (plist-get r :tags)))
                      (and (listp tags)
                           (seq-some (lambda (want)
                                       (seq-some (lambda (have) (equal (format "%s" have) want)) tags))
                                     ert-flow--panel-tags-filter))))
                  rs)))
      rs)))

(defun ert-flow-panel-filter--set-status (statuses)
  "Helper: set STATUS filter to STATUSES (list or nil) and re-render."
  (setq-local ert-flow--panel-status-filter statuses)
  (message "ert-flow: status filter → %s"
           (if statuses (mapconcat (lambda (s) (format "%s" s)) statuses ",") "all"))
  (ert-flow--render))

(defun ert-flow-panel-filter-pass ()  (interactive) (ert-flow-panel-filter--set-status '(pass)))
(defun ert-flow-panel-filter-fail ()  (interactive) (ert-flow-panel-filter--set-status '(fail)))
(defun ert-flow-panel-filter-error () (interactive) (ert-flow-panel-filter--set-status '(error)))
(defun ert-flow-panel-filter-skip ()  (interactive) (ert-flow-panel-filter--set-status '(skip xfail)))
(defun ert-flow-panel-filter-all ()   (interactive) (ert-flow-panel-filter--set-status nil))

(defun ert-flow-panel-set-name-filter (re)
  "Prompt for name regexp RE and re-render; empty input clears the filter."
  (interactive (list (read-string "Filter name (regexp, empty=clear): " ert-flow--panel-name-regexp)))
  (setq-local ert-flow--panel-name-regexp (if (string-empty-p re) nil re))
  (message "ert-flow: name filter → %s" (or ert-flow--panel-name-regexp "none"))
  (ert-flow--render))

(defun ert-flow-panel-set-tags-filter (tags)
  "Prompt for TAGS (comma-separated) and re-render; empty input clears the filter."
  (interactive (list (read-string "Filter tags (comma-separated, empty=clear): "
                                  (when (and (listp ert-flow--panel-tags-filter)
                                             ert-flow--panel-tags-filter)
                                    (mapconcat #'identity ert-flow--panel-tags-filter ",")))))
  (let ((trim (string-trim tags)))
    (setq-local ert-flow--panel-tags-filter
                (if (string-empty-p trim)
                    nil
                  (seq-filter (lambda (s) (not (string-empty-p s)))
                              (mapcar #'string-trim (split-string trim ","))))))
  (message "ert-flow: tags filter → %s"
           (if ert-flow--panel-tags-filter
               (mapconcat #'identity ert-flow--panel-tags-filter ",")
             "none"))
  (ert-flow--render))

(defun ert-flow-panel-filter-clear ()
  "Clear all panel filters and re-render."
  (interactive)
  (setq-local ert-flow--panel-status-filter nil)
  (setq-local ert-flow--panel-name-regexp nil)
  (message "ert-flow: filters cleared")
  (ert-flow--render))

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
  "Insert single test line for result plist R with properties.

Preserve special font family (e.g. Material Icons) supplied by icon glyphs
while applying the status face (color/weight). We compose a face list
so the icon's font-family is kept and our face supplies the foreground.

Display name: for better readability we strip the common \"ert-flow/\" prefix
from test names for display only (the underlying result plist is unchanged)."
  (let* ((st (plist-get r :status))
         (nm (plist-get r :name))
         ;; Shorten names that are in the form 'ert-flow/...' for display.
         (display-nm (if (and (stringp nm) (string-prefix-p "ert-flow/" nm))
                         (substring nm (length "ert-flow/"))
                       nm))
         (icon (if ert-flow-icons (ert-flow--status-icon st) ""))
         (face (ert-flow--status-face st))
         ;; If the icon string has a face (from all-the-icons), keep it and combine
         (icon-face (and (> (length icon) 0) (get-text-property 0 'face icon)))
         (combined-face (if icon-face (list icon-face face) face))
         (icon-prop (if (> (length icon) 0)
                        (propertize icon 'face combined-face)
                      ""))
         (line (format "  %s %s\n" icon-prop (or display-nm nm))))
    (insert (ert-flow--propertize line r))))

(defun ert-flow--suite-aggregate (results)
  "Return aggregate status symbol for RESULTS: all-pass, all-fail, mixed, skipped-only, or empty."
  (let* ((p (cl-count-if (lambda (r) (eq (plist-get r :status) 'pass)) results))
         (f (cl-count-if (lambda (r) (eq (plist-get r :status) 'fail)) results))
         (e (cl-count-if (lambda (r) (eq (plist-get r :status) 'error)) results))
         (s (cl-count-if (lambda (r) (memq (plist-get r :status) '(skip xfail))) results)))
    (cond
     ((and (= p 0) (= f 0) (= e 0) (= s 0)) 'empty)
     ((and (> p 0) (= f 0) (= e 0)) 'all-pass)
     ((and (= p 0) (> (+ f e) 0)) 'all-fail)
     ((and (= p 0) (= (+ f e) 0) (> s 0)) 'skipped-only)
     (t 'mixed))))

(defun ert-flow--suite-icon (agg)
  "Return colored folder icon string for AGG aggregate status."
  (let* ((color (pcase agg
                  ('all-pass "SpringGreen3")
                  ('all-fail "Red3")
                  ('mixed "DarkOrange2")
                  ('skipped-only "gray60")
                  (_ "gray60")))
         (face `(:foreground ,color))
         (icon
          (cond
           ((and (featurep 'all-the-icons)
                 (fboundp 'all-the-icons-material)
                 (display-graphic-p)
                 (find-font (font-spec :family "Material Icons")))
            (all-the-icons-material "folder" :v-adjust 0.0 :height 1.0 :face face))
           ((char-displayable-p ?📁) "📁")
           (t "[+]"))))
    (if (stringp icon) (propertize icon 'face face) icon)))

(defun ert-flow--insert-suite (suite results)
  "Insert a SUITE heading and its RESULTS, always showing suite header.
Heading is clickable to toggle fold. Header includes a colored folder icon
reflecting aggregate suite status:
- green: all tests passed
- yellow: mixed (some passed, some failed/errors)
- red: all failed/errors
- gray: only skipped/xfail or empty.

Note: do not override icon color with a uniform button face."
  (ert-flow--ensure-fold-table)
  (let* ((agg (ert-flow--suite-aggregate results))
         ;; Initialize default folding once per suite (only if key absent)
         (present (let ((marker '#:no))
                    (not (eq (gethash suite ert-flow--folded-suites marker) marker))))
         (_init (unless present
                  (when (eq agg 'all-pass)
                    (puthash suite t ert-flow--folded-suites))))
         (folded (gethash suite ert-flow--folded-suites))
         (arrow (if folded "▸" "▾"))
         (icon (ert-flow--suite-icon agg))
         (name suite)
         (s (concat arrow " " icon " " name "\n"))
         (arrow-len (length arrow))
         (icon-len (length icon))
         (name-start (+ arrow-len 1 icon-len 1))
         (name-end (1- (length s))))
    ;; Bold arrow and suite name; icon keeps its own colored face
    (add-text-properties 0 arrow-len '(face bold) s)
    (when (> name-end name-start)
      (add-text-properties name-start name-end '(face bold) s))
    ;; Only highlight and show hand cursor over the group name (not arrow/icon)
    (when (> name-end name-start)
      (add-text-properties name-start name-end '(mouse-face highlight pointer hand) s))
    (insert-text-button
     s
     'face '(:underline nil)  ;; remove underline, do not override icon color
     'mouse-face nil
     'follow-link t
     'help-echo "Toggle group (mouse-1, TAB)"
     'keymap ert-flow--suite-button-map
     'ert-flow--suite suite
     'action (lambda (_btn)
               (ert-flow--ensure-fold-table)
               (let ((cur (gethash suite ert-flow--folded-suites)))
                 (puthash suite (not cur) ert-flow--folded-suites))
               ;; Ensure we re-render the correct panel buffer (current buffer).
               (let ((ert-flow--panel-buffer-name (buffer-name)))
                 (setq-local ert-flow--restore-point-suite suite)
                 (ert-flow--render))))
    (unless folded
      (dolist (r results)
        (ert-flow--insert-test-line r)))))

(defun ert-flow--goto-suite-heading (suite)
  "Move point to the heading line of SUITE. Return non-nil on success."
  (goto-char (point-min))
  (catch 'found
    (while (not (eobp))
      (when (equal (get-text-property (line-beginning-position) 'ert-flow--suite) suite)
        (beginning-of-line)
        (throw 'found t))
      (forward-line 1))
    nil))

(defun ert-flow--render-context--simple ()
  "Deprecated pre-logging variant; use `ert-flow--render-context' instead."
  (ert-flow--render-context))

;; Collapsible Status block (panel header area in buffer)
(defvar-local ert-flow--panel-status-folded nil
  "If non-nil, the status block is folded in this panel buffer.")

(defun ert-flow--panel-status-icon ()
  "Return an icon for the Status block (all-the-icons if available, else text).

Preserve the icon's font family from all-the-icons so the glyph renders
with the correct font; only add our foreground color on top."
  (cond
   ((and (featurep 'all-the-icons)
         (fboundp 'all-the-icons-material)
         (display-graphic-p)
         (find-font (font-spec :family "Material Icons")))
    ;; Ask all-the-icons to apply the foreground; this keeps the Material Icons family.
    (all-the-icons-material "assignment" :v-adjust 0.02 :height 1.0
                            :face '(:foreground "#b3cfff")))
   ((char-displayable-p ?📝) "📝")
   (t "[S]")))

(defun ert-flow--panel-summary-icon ()
  "Return an icon for the Summary header."
  (cond
   ((and (featurep 'all-the-icons)
         (fboundp 'all-the-icons-material)
         (display-graphic-p)
         (find-font (font-spec :family "Material Icons")))
    (all-the-icons-material "view_list" :v-adjust 0.02 :height 1.0
                            :face '(:foreground "gray70")))
   ((char-displayable-p ?≡) "≡")
   (t "[Σ]")))

(defun ert-flow--status-line-icon (key &optional state)
  "Return icon for Status line KEY. Optional STATE for toggles like watch."
  (let ((gui (and (featurep 'all-the-icons)
                  (display-graphic-p)
                  (find-font (font-spec :family "Material Icons")))))
    (cond
     (gui
      (pcase key
        ('counters (all-the-icons-material "subject" :height 1.0 :v-adjust 0.02
                                           :face '(:foreground "LightSkyBlue3")))
        ('duration (all-the-icons-material "timer" :height 1.0 :v-adjust 0.02
                                           :face '(:foreground "gray70")))
        ('proc     (all-the-icons-material "autorenew" :height 1.0 :v-adjust 0.02
                                           :face '(:foreground "MediumPurple3")))
        ('project  (all-the-icons-material "folder_open" :height 1.0 :v-adjust 0.02
                                           :face '(:foreground "SteelBlue3")))
        ('runner   (if (fboundp 'all-the-icons-octicon)
                       (all-the-icons-octicon "rocket" :height 1.0 :v-adjust 0.02
                                              :face '(:foreground "Gold3"))
                     (all-the-icons-material "rocket_launch" :height 1.0 :v-adjust 0.02
                                             :face '(:foreground "Gold3"))))
        ('mode     (all-the-icons-material "sync" :height 1.0 :v-adjust 0.02
                                           :face '(:foreground "DarkOrange2")))
        ('watch    (all-the-icons-material (if (eq state 'on) "visibility" "visibility_off")
                                           :height 1.0 :v-adjust 0.02
                                           :face (if (eq state 'on)
                                                     '(:foreground "DeepSkyBlue3")
                                                   '(:foreground "gray60"))))
        ('parser   (all-the-icons-material "extension" :height 1.0 :v-adjust 0.02
                                           :face '(:foreground "SlateGray3")))
        (_ "")))
     (t
      (pcase key
        ('counters (if (char-displayable-p ?📊) "📊" "Σ"))
        ('duration (if (char-displayable-p ?⏱) "⏱" "T"))
        ('proc     (if (char-displayable-p ?⚙) "⚙" "P"))
        ('project  (if (char-displayable-p ?📁) "📁" "D"))
        ('runner   (if (char-displayable-p ?🚀) "🚀" "R"))
        ('mode     (if (char-displayable-p ?⟳) "⟳" "M"))
        ('watch    (if (eq state 'on)
                       (if (char-displayable-p ?👁) "👁" "W")
                     (if (char-displayable-p ?🙈) "🙈" "w")))
        ('parser   (if (char-displayable-p ?🧩) "🧩" "X"))
        (_ ""))))))

(defun ert-flow--status-counters-str (sum results)
  "Return colored counters string: \"N (P:x F:y E:z S:u U:w)\"."
  (cl-destructuring-bind (&key total unexpected passed failed error skipped &allow-other-keys)
      (apply #'ert-flow--summary-counters (list sum results))
    (let* ((u-face (if (> (or unexpected 0) 0) 'ert-flow-face-fail 'ert-flow-face-pass)))
      (concat
       (format "%d (" (or total 0))
       (propertize (format "P:%d" (or passed 0)) 'face 'ert-flow-face-pass)
       " "
       (propertize (format "F:%d" (or failed 0)) 'face 'ert-flow-face-fail)
       " "
       (propertize (format "E:%d" (or error 0)) 'face 'ert-flow-face-error)
       " "
       (propertize (format "S:%d" (or skipped 0)) 'face 'ert-flow-face-skip)
       " "
       (propertize (format "U:%d" (or unexpected 0)) 'face u-face)
       ")"))))

(defun ert-flow--insert-status-block (sess sum results)
  "Insert the collapsible Status block with meta info and counters."
  (let* ((folded ert-flow--panel-status-folded)
         (arrow (if folded "▸" "▾"))
         (icon (ert-flow--panel-status-icon))
         (project (file-name-nondirectory
                   (directory-file-name (ert-flow--session-root sess))))
         (runner-sym (ert-flow--conf sess 'runner ert-flow-runner))
         (runner (if (eq runner-sym 'in-emacs-ert) "in-emacs" "external"))
         (watch-on (ert-flow--get-watch-enabled sess))
         (watch (if watch-on "On" "Off"))
         (mode (ert-flow--conf sess 'watch-mode ert-flow-watch-mode))
         (parser-used (or (ert-flow--get-last-parser sess)
                          (ert-flow--conf sess 'parser ert-flow-parser)))
         (parser-str (format "%s" parser-used))
         (active ert-flow--active-run-count)
         (queued (length ert-flow--run-queue))
         (dur-str (plist-get (apply #'ert-flow--summary-counters (list sum results)) :dur-str))
         ;; Compose heading: avoid adding (propertize ...) to the *whole* string, only apply :weight bold to word "Status"
         (arrow-len (length arrow))
         (icon-len (length icon))
         (status-word " Status")
         (label (concat arrow " " icon status-word))
         (label-str
          (copy-sequence
           (concat label
                   (when folded
                     (concat " "
                             (ert-flow--status-counters-str sum results)))
                   "\n")))
         (status-pos (string-match "Status" label-str)))
    ;; Make only "Status" bold, not the whole string (to avoid underline/other theme effects)
    (when status-pos
      (add-text-properties status-pos (+ status-pos (length "Status"))
                           '(face (:weight bold)) label-str)
      ;; Only highlight and show hand cursor over the word "Status"
      (add-text-properties status-pos (+ status-pos (length "Status"))
                           '(mouse-face highlight pointer hand) label-str))
    ;; Heading with toggle
    (insert-text-button
     label-str
     'face '(:underline nil)  ;; remove underline, keep icon color (no foreground here)
     'mouse-face nil
     'follow-link t
     'help-echo "Fold/unfold Status (mouse-1, TAB)"
     'keymap ert-flow--status-button-map
     'ert-flow--nav 'status
     'action (lambda (_btn)
               (ert-flow-toggle-status)))
    ;; Body when unfolded (each line with its own icon)
    (unless folded
      (let ((mk (lambda (icon-str body)
                  (insert (propertize (format "  %s %s" icon-str body)
                                      'ert-flow--nav 'status-item))
                  (insert "\n"))))
        (funcall mk (ert-flow--status-line-icon 'counters)
                 (propertize (ert-flow--status-counters-str sum results) 'face '(:weight bold)))
        (funcall mk (ert-flow--status-line-icon 'duration)
                 (format "duration: %s" (or dur-str "-")))
        (funcall mk (ert-flow--status-line-icon 'proc)
                 (format "Proc: active %d, queued %d" active queued))
        (funcall mk (ert-flow--status-line-icon 'project)
                 (format "Project: %s" project))
        (funcall mk (ert-flow--status-line-icon 'runner)
                 (format "Runner: %s" runner))
        (funcall mk (ert-flow--status-line-icon 'mode)
                 (format "Mode: %s" mode))
        (funcall mk (ert-flow--status-line-icon 'watch (if watch-on 'on 'off))
                 (format "Watch: %s" watch))
        (funcall mk (ert-flow--status-line-icon 'parser)
                 (format "Parser: %s" parser-str))))
    (insert "\n")))

(defun ert-flow-toggle-status ()
  "Toggle folding of the Status block and re-render."
  (interactive)
  (setq ert-flow--panel-status-folded (not ert-flow--panel-status-folded))
  (let ((ert-flow--panel-buffer-name (buffer-name)))
    (ert-flow--render)))

(defun ert-flow--render-insert--precoverage (ctx)
  "Deprecated pre-coverage variant: kept for reference."
  (ert-flow--render-insert ctx))

(defun ert-flow--render-restore-point ()
  "Restore point to requested suite heading, or move to beginning."
  (if (and (boundp 'ert-flow--restore-point-suite) ert-flow--restore-point-suite)
      (progn
        (ert-flow--goto-suite-heading ert-flow--restore-point-suite)
        (setq ert-flow--restore-point-suite nil))
    (goto-char (point-min))))

(defun ert-flow--render-context ()
  "Collect session context for rendering.
Returns plist: (:sess :sum :results :proc) and emits diagnostic logs."
  (let* ((sess (ert-flow--find-panel-session))
         (sum (and sess (ert-flow--get-last-summary sess)))
         (results (and sess (ert-flow--get-last-results sess)))
         (proc (and sess (ert-flow--get-process sess))))
    (ert-flow--log "render: panel=%s sess=%s results=%s total=%s filters: status=%S name=%S tags=%S"
                   ert-flow--panel-buffer-name
                   (and sess (ert-flow--dbg-sess sess))
                   (if (listp results) (number-to-string (length results)) "nil")
                   (or (and (listp sum) (alist-get 'total sum)) "?")
                   (and (boundp 'ert-flow--panel-status-filter) ert-flow--panel-status-filter)
                   (and (boundp 'ert-flow--panel-name-regexp) ert-flow--panel-name-regexp)
                   (and (boundp 'ert-flow--panel-tags-filter) ert-flow--panel-tags-filter))
    (when (null results)
      (ert-flow--log "no-results: none stored yet (root=%s) proc-live=%s first-open-done=%s run-on-open=%s runner=%s cmd=%S active=%d queued=%d"
                     (and sess (ert-flow--session-root sess))
                     (and proc (process-live-p proc))
                     (ert-flow--conf sess 'first-open-run-done nil)
                     ert-flow-run-on-open
                     (ert-flow--conf sess 'runner ert-flow-runner)
                     (ert-flow--conf sess 'external-command ert-flow-external-command)
                     ert-flow--active-run-count
                     (length ert-flow--run-queue))
      (ert-flow--log-concurrency-state))
    (when (and (listp results) (= (length results) 0))
      (ert-flow--log "no-results: empty list (root=%s) total=%s last-parser=%s"
                     (and sess (ert-flow--session-root sess))
                     (or (and (listp sum) (alist-get 'total sum)) "?")
                     (or (ert-flow--get-last-parser sess)
                         (ert-flow--conf sess 'parser ert-flow-parser))))
    (list :sess sess :sum sum :results results :proc proc)))

(defun ert-flow--render-insert (ctx)
  "Insert Status block and grouped suites using CTX."
  (let* ((sess (plist-get ctx :sess))
         (sum (plist-get ctx :sum))
         (results (plist-get ctx :results)))
    ;; Header-line hosts controls; buffer body starts with Status block.
    (ert-flow--insert-status-block sess sum results)
    ;; Optional coverage block (if ert-flow-coverage is loaded)
    (when (fboundp 'ert-flow-coverage--insert-panel-block)
      (ignore-errors (ert-flow-coverage--insert-panel-block sess)))
    ;; Directly list groups (Summary header removed as redundant)
    (dolist (pair (ert-flow--group-results (ert-flow--apply-panel-filters results)))
      (ert-flow--insert-suite (car pair) (cdr pair)))))

(defun ert-flow--render-restore-point ()
  "Restore point after rendering to suite header if requested, else to beginning."
  (if (and (boundp 'ert-flow--restore-point-suite) ert-flow--restore-point-suite)
      (progn
        (ert-flow--goto-suite-heading ert-flow--restore-point-suite)
        (setq ert-flow--restore-point-suite nil))
    (goto-char (point-min))))

(defun ert-flow--render ()
  "Render the panel for the session associated with `ert-flow--panel-buffer-name'."
  (let ((buf (get-buffer-create ert-flow--panel-buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (let ((ctx (ert-flow--render-context)))
          (ert-flow--render-insert ctx))
        (ert-flow--render-restore-point)))))


(defun ert-flow--details-insert-toolbar (name details)
  "Insert toolbar for details buffer with NAME and DETAILS."
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
  (insert "\n\n"))

(defun ert-flow--details-populate-buffer (buf name details)
  "Populate BUF with toolbar and DETAILS for NAME."
  (with-current-buffer buf
    (let ((inhibit-read-only t))
      (read-only-mode -1)
      (erase-buffer)
      (ert-flow--details-insert-toolbar name details)
      (insert details)
      (goto-char (point-min))
      (view-mode 1))))

;;;###autoload
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
        (ert-flow--details-populate-buffer buf name details)
        (display-buffer buf)))))

;;; Navigation

(defun ert-flow--goto-next-item (dir)
  "Move point to next (DIR>0) or previous (DIR<0) item (test, suite, status/summary)."
  (let ((step (if (> dir 0) 1 -1))
        (start (point))
        (done nil))
    (catch 'jump
      (while (if (> dir 0) (not (eobp)) (not (bobp)))
        (forward-line step)
        (let ((is-test (get-text-property (line-beginning-position) 'ert-flow--result))
              (is-suite (get-text-property (line-beginning-position) 'ert-flow--suite))
              (nav     (get-text-property (line-beginning-position) 'ert-flow--nav)))
          (when (or nav is-test is-suite)
            (beginning-of-line)
            (setq done t)
            (throw 'jump t)))))
    (unless done
      (goto-char start)
      (message "ert-flow: no more items"))))

(defun ert-flow-next-item () (interactive) (ert-flow--goto-next-item 1))
(defun ert-flow-previous-item () (interactive) (ert-flow--goto-next-item -1))

(defun ert-flow-tab ()
  "Context-aware TAB: toggle Status at Status header, on any Status line, or toggle suite."
  (interactive)
  (let* ((nav (get-text-property (line-beginning-position) 'ert-flow--nav))
         (suite (get-text-property (line-beginning-position) 'ert-flow--suite)))
    (cond
     ((or (eq nav 'status) (eq nav 'status-item))
      (ert-flow-toggle-status))
     (suite
      (ert-flow-toggle-group-at-point))
     (t
      (ert-flow-toggle-group-at-point)))))

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
(defun ert-flow--failed-names (results)
  "Return names of failed/error tests from RESULTS."
  (mapcar (lambda (r) (plist-get r :name))
          (seq-filter (lambda (r) (memq (plist-get r :status) '(fail error)))
                      (or results '()))))

(defun ert-flow--run-failed-in-emacs (sess fails)
  "Schedule in-Emacs run for FAILS in SESS."
  (let ((selector (ert-flow--selector-names fails))
        (label (format "failed(in-emacs) %d" (length fails))))
    (ert-flow--maybe-start-thunk
     sess label
     (lambda () (ert-flow--start-in-emacs sess selector label)))))

(defun ert-flow--build-external-failed-cmd (sess fails)
  "Return external argv to run FAILS for SESS or nil if unavailable."
  (let* ((ext-cmd (ert-flow--conf sess 'external-command ert-flow-external-command))
         (failed-fn (ert-flow--conf sess 'external-failed-args-function ert-flow-external-failed-args-function)))
    (when (and (listp ext-cmd) failed-fn)
      (let ((extra (ignore-errors (funcall failed-fn fails))))
        (when (and (listp extra) (seq-every-p #'stringp extra))
          (append ext-cmd extra))))))

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
         (fails (ert-flow--failed-names results))
         (runner (ert-flow--conf sess 'runner ert-flow-runner)))
    (cond
     ((null fails)
      (message "ert-flow: no failed tests to run")
      (ert-flow-run))
     ((eq runner 'in-emacs-ert)
      (ert-flow--run-failed-in-emacs sess fails))
     (t
      (let ((cmd (ert-flow--build-external-failed-cmd sess fails)))
        (if (not cmd)
            (progn
              (message "ert-flow: cannot run failed selectively; running all")
              (ert-flow-run))
          (ert-flow--maybe-start-run sess cmd "failed")))))))

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
       (ert-flow--log "run: in-emacs (root=%s)" root)
       (let ((selector t) (label "all(in-emacs)"))
         (ert-flow--maybe-start-thunk
          sess label
          (lambda () (ert-flow--start-in-emacs sess selector label)))))
      (_
       (let* ((ext (ert-flow--conf sess 'external-command ert-flow-external-command))
              (cmd (ert-flow--normalize-command ext)))
         (unless cmd
           (ert-flow--log "run: external-command missing (root=%s ext=%S)" root ext)
           (user-error "Set per-session external command (M-x ert-flow-detect-runner)"))
         (ert-flow--log "run: external (root=%s cmd=%S)" root cmd)
         (ert-flow--maybe-start-run sess cmd "all"))))))

(defun ert-flow--proc-filter (proc chunk)
  "Accumulate CHUNK from the running process, session-aware."
  (condition-case err
      (let* ((sess (process-get proc 'ert-flow-session))
             (old (and sess (ert-flow--get-last-raw-output sess)))
             (combined (concat (or old "") chunk)))
        ;; Trim per configured cap to save memory
        (when (and (integerp ert-flow-max-raw-output-bytes)
                   (> (length combined) ert-flow-max-raw-output-bytes))
          (setq combined (substring combined (- (length combined) ert-flow-max-raw-output-bytes))))
        (when sess
          (ert-flow--set-last-raw-output sess combined))
        ;; Legacy global mirrors current session
        (setq ert-flow--last-raw-output combined))
    (error
     (ert-flow--log "Filter error: %S" err))))

(defun ert-flow--looks-like-batch-output (s)
  "Heuristic: return non-nil if S looks like ERT batch output."
  (and (stringp s)
       (or (string-match-p "^Running[ \t]+[0-9]+[ \t]+tests?" s)
           (string-match-p "^Ran[ \t]+[0-9]+[ \t]+tests?" s)
           (string-match-p "^Test[ \t]+" s)
           (string-match-p "^[ \t]*\\(FAILED\\|ERROR\\|SKIPPED\\|XFAIL\\|XPASS\\)[ \t]" s)
           (string-match-p "^[ \t]*passed[ \t]+[0-9]+/[0-9]+[ \t]+" s))))

(defun ert-flow--looks-like-json-output (s)
  "Heuristic: return non-nil if S looks like our JSON payload."
  (and (stringp s)
       (string-match-p "{" s)
       (or (string-match-p "\"tests\"" s)
           (string-match-p "\"summary\"" s))))

(defun ert-flow--choose-output-for-parse (stdout stderr)
  "Choose stream to parse using simple heuristics.

Prefer JSON when only one side looks like JSON.
If both sides look like ERT batch output, merge them (stdout + stderr),
because ERT often splits progress and failures across streams.
Otherwise, fall back to non-empty stdout, then stderr."
  (cond
   ;; Prefer JSON stream when only one side looks like JSON
   ((and (ert-flow--looks-like-json-output stdout)
         (not (ert-flow--looks-like-json-output stderr)))
    stdout)
   ((and (ert-flow--looks-like-json-output stderr)
         (not (ert-flow--looks-like-json-output stdout)))
    stderr)
   ;; If both look like batch output → merge
   ((and (ert-flow--looks-like-batch-output stdout)
         (ert-flow--looks-like-batch-output stderr))
    (concat (or stdout "") "\n" (or stderr "")))
   ;; Otherwise prefer the side that looks like ERT batch output
   ((and (ert-flow--looks-like-batch-output stdout)
         (not (ert-flow--looks-like-batch-output stderr)))
    stdout)
   ((and (ert-flow--looks-like-batch-output stderr)
         (not (ert-flow--looks-like-batch-output stdout)))
    stderr)
   ;; Fallbacks
   ((and (stringp stdout) (> (length stdout) 0)) stdout)
   ((and (stringp stderr) (> (length stderr) 0)) stderr)
   (t "")))

(defun ert-flow--sentinel-flush ()
  "Give the process filter a brief chance to flush remaining output."
  (dotimes (_ 3) (accept-process-output nil 0.05)))

(defun ert-flow--sentinel-read-streams (proc)
  "Return plist with session and streams for PROC: (:sess :root :stdout :stderr :stderr-buf)."
  (let* ((sess (process-get proc 'ert-flow-session))
         (root (and sess (ert-flow--session-root sess)))
         (stdout (or (and sess (ert-flow--get-last-raw-output sess))
                     ert-flow--last-raw-output))
         (stderr-buf (process-get proc 'ert-flow-stderr-buf))
         (stderr-str (when (buffer-live-p stderr-buf)
                       (with-current-buffer stderr-buf (buffer-string)))))
    (list :sess sess :root root :stdout stdout :stderr stderr-str :stderr-buf stderr-buf)))

(defun ert-flow--sentinel-parse (sess raw)
  "Parse RAW according to SESS parser preference. Return (used summary results)."
  (let* ((pmode (ert-flow--conf sess 'parser ert-flow-parser))
         used summary results)
    (pcase pmode
      ('json
       (setq used 'json)
       (let ((pair (ert-flow--parse-json-output raw)))
         (when pair (setq summary (car pair) results (cdr pair)))))
      ('ert-batch
       (setq used 'ert-batch)
       (let ((pair (ert-flow--parse-batch-output raw)))
         (setq summary (car pair) results (cdr pair))))
      (_
       (let ((pair (ert-flow--parse-json-output raw)))
         (if pair
             (progn
               (setq used 'json summary (car pair) results (cdr pair)))
           (setq used 'ert-batch)
           (let ((p2 (ert-flow--parse-batch-output raw)))
             (setq summary (car p2) results (cdr p2)))))))
    (list used summary results)))

(defun ert-flow--sentinel-store (sess used summary results stderr-str)
  "Store RESULTS and SUMMARY into SESS (and globals), trimming STDERR-STR."
  (when sess
    (when (and (stringp stderr-str)
               (integerp ert-flow-max-raw-output-bytes)
               (> (length stderr-str) ert-flow-max-raw-output-bytes))
      (setq stderr-str (substring stderr-str (- (length stderr-str) ert-flow-max-raw-output-bytes))))
    (ert-flow--set-last-stderr-output sess stderr-str)
    (ert-flow--touch-session sess)
    (ert-flow--set-last-summary sess summary)
    (ert-flow--set-last-results sess results)
    (ert-flow--set-process sess nil)
    (ert-flow--set-last-parser sess used)
    (ert-flow--log "store: root=%s parser=%s results=%d total=%s stderr-len=%s"
                   (ert-flow--session-root sess) used (length results)
                   (or (and (listp summary) (alist-get 'total summary)) "?")
                   (if (stringp stderr-str) (number-to-string (length stderr-str)) "nil")))
  (setq ert-flow--last-summary summary
        ert-flow--last-results results))

(defun ert-flow--sentinel-render (root)
  "Re-render panel for ROOT."
  (let* ((bufname (ert-flow--session-panel-name (or root (ert-flow--project-root)))))
    (let ((ert-flow--panel-buffer-name bufname))
      (ert-flow--render))))

(defun ert-flow--proc-sentinel (proc event)
  "Handle process EVENT (session-aware)."
  (condition-case err
      (progn
        (ert-flow--log "Sentinel: %s" (string-trim (or event "")))
        (when (and (stringp event)
                   (string-match-p "\\(finished\\|exited\\)" event))
          (ert-flow--sentinel-flush)
          (pcase-let* ((plist (ert-flow--sentinel-read-streams proc))
                       (sess (plist-get plist :sess))
                       (root (plist-get plist :root))
                       (stdout (plist-get plist :stdout))
                       (stderr-str (plist-get plist :stderr))
                       (stderr-buf (plist-get plist :stderr-buf))
                       (raw (ert-flow--choose-output-for-parse stdout stderr-str)))
            (ert-flow--log "sentinel: root=%s label=%s" root (process-get proc 'ert-flow-label))
            (ert-flow--log "sentinel: stdout len=%s stderr len=%s → using=%s"
                           (if (stringp stdout) (number-to-string (length stdout)) "nil")
                           (if (stringp stderr-str) (number-to-string (length stderr-str)) "nil")
                           (if (eq raw stdout) "stdout" (if (eq raw stderr-str) "stderr" "empty")))
            (pcase-let ((`(,used ,summary ,results) (ert-flow--sentinel-parse sess raw)))
              (ert-flow--log "sentinel parsed: results=%d total=%s (parser=%s)"
                             (length results)
                             (or (and (listp summary) (alist-get 'total summary)) "?")
                             (or used (ert-flow--conf sess 'parser ert-flow-parser)))
              (ert-flow--sentinel-store sess used summary results stderr-str)
              ;; Auto-load coverage (LCOV) if the module is present and enabled.
              (when (and (featurep 'ert-flow-coverage)
                         (boundp 'ert-flow-coverage-auto-load)
                         ert-flow-coverage-auto-load)
                (ignore-errors (ert-flow-coverage-load t)))
              (when (buffer-live-p stderr-buf) (kill-buffer stderr-buf))
              (ert-flow--sentinel-render root)
              (ert-flow--finish-run)))))
    (error
     (ert-flow--log "Sentinel error: %S" err))))

;;;; Runner helpers

(defun ert-flow--ensure-fold-table ()
  "Ensure the panel buffer has a fold table initialized."
  (unless (hash-table-p ert-flow--folded-suites)
    (setq-local ert-flow--folded-suites (make-hash-table :test 'equal))))

;;;###autoload
(defun ert-flow-toggle-group-at-point ()
  "Toggle folding of suite group at point (on header) or the group containing the current test line.
Does nothing when point is not on a suite header or test line."
  (interactive)
  (ert-flow--ensure-fold-table)
  (let* ((suite-here (get-text-property (line-beginning-position) 'ert-flow--suite))
         (test-here (get-text-property (line-beginning-position) 'ert-flow--result))
         (suite (cond
                 (suite-here suite-here)
                 (test-here
                  (save-excursion
                    (beginning-of-line)
                    (catch 'found
                      (while (not (bobp))
                        (forward-line -1)
                        (let ((s (get-text-property (line-beginning-position) 'ert-flow--suite)))
                          (when s (throw 'found s))))
                      nil)))
                 (t nil))))
    (when suite
      (let ((cur (gethash suite ert-flow--folded-suites)))
        (puthash suite (not cur) ert-flow--folded-suites))
      ;; Re-render the panel corresponding to the current buffer and restore point.
      (let ((ert-flow--panel-buffer-name (buffer-name)))
        (setq-local ert-flow--restore-point-suite suite)
        (ert-flow--render)))))

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
  ;; Re-render the panel corresponding to the current buffer.
  (let ((ert-flow--panel-buffer-name (buffer-name)))
    (ert-flow--render)))

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
  last-parser
  last-activity-at)

;; cl-defstruct provides setf accessors for session slots; custom gv-setters removed.

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

(defun ert-flow--get-last-parser (sess)
  "Safe accessor for session's last parser symbol.
Returns nil if the slot is missing (older struct instances)."
  (or (condition-case nil
          (ert-flow--session-last-parser sess)
        (error nil))
      (ert-flow--conf sess 'last-parser nil)))

(defun ert-flow--set-last-parser (sess value)
  "Safe setter for session's last parser symbol.
If the slot is missing (older struct instances), stores VALUE in session config."
  (condition-case nil
      (setf (ert-flow--session-last-parser sess) value)
    (error (ert-flow--set-conf sess 'last-parser value)))
  value)

(defun ert-flow--get-last-activity-at (sess)
  "Safe accessor for session's last activity time.
Returns nil if the slot is missing (older struct instances)."
  (or (condition-case nil
          (ert-flow--session-last-activity-at sess)
        (error nil))
      (ert-flow--conf sess 'last-activity-at nil)))

(defun ert-flow--set-last-activity-at (sess value)
  "Safe setter for session's last activity time.
If the slot is missing (older struct instances), stores VALUE in session config."
  (condition-case nil
      (setf (ert-flow--session-last-activity-at sess) value)
    (error (ert-flow--set-conf sess 'last-activity-at value)))
  value)

;; Safe getter/setter for watch-enabled
(defun ert-flow--get-watch-enabled (sess)
  (or (condition-case nil
          (ert-flow--session-watch-enabled sess)
        (error nil))
      (ert-flow--conf sess 'watch-enabled nil)))

(defun ert-flow--set-watch-enabled (sess value)
  (condition-case nil
      (setf (ert-flow--session-watch-enabled sess) value)
    (error nil))
  (ert-flow--set-conf sess 'watch-enabled value)
  value)

;; Safe session getters/setters for volatile fields: summary/results/stdout/stderr/process.
;; They write through to struct slots when possible and always mirror into session config.
(defun ert-flow--get-last-summary (sess)
  (or (condition-case nil
          (ert-flow--session-last-summary sess)
        (error nil))
      (ert-flow--conf sess 'last-summary nil)))

(defun ert-flow--set-last-summary (sess value)
  (condition-case nil
      (setf (ert-flow--session-last-summary sess) value)
    (error nil))
  (ert-flow--set-conf sess 'last-summary value)
  value)

(defun ert-flow--get-last-results (sess)
  (or (condition-case nil
          (ert-flow--session-last-results sess)
        (error nil))
      (ert-flow--conf sess 'last-results nil)))

(defun ert-flow--set-last-results (sess value)
  (condition-case nil
      (setf (ert-flow--session-last-results sess) value)
    (error nil))
  (ert-flow--set-conf sess 'last-results value)
  value)

(defun ert-flow--get-last-raw-output (sess)
  (or (condition-case nil
          (ert-flow--session-last-raw-output sess)
        (error nil))
      (ert-flow--conf sess 'last-raw-output nil)))

(defun ert-flow--set-last-raw-output (sess value)
  (condition-case nil
      (setf (ert-flow--session-last-raw-output sess) value)
    (error nil))
  (ert-flow--set-conf sess 'last-raw-output value)
  value)

(defun ert-flow--get-last-stderr-output (sess)
  (or (condition-case nil
          (ert-flow--session-last-stderr-output sess)
        (error nil))
      (ert-flow--conf sess 'last-stderr-output nil)))

(defun ert-flow--set-last-stderr-output (sess value)
  (condition-case nil
      (setf (ert-flow--session-last-stderr-output sess) value)
    (error nil))
  (ert-flow--set-conf sess 'last-stderr-output value)
  value)

(defun ert-flow--get-process (sess)
  (or (condition-case nil
          (ert-flow--session-process sess)
        (error nil))
      (ert-flow--conf sess 'process nil)))

(defun ert-flow--set-process (sess value)
  (condition-case nil
      (setf (ert-flow--session-process sess) value)
    (error nil))
  (ert-flow--set-conf sess 'process value)
  value)

(defvar-local ert-flow--folded-suites nil
  "Hash-table of folded suite names (buffer-local in panel buffers).")

(defvar-local ert-flow--panel-status-filter nil
  "If non-nil, a list of status symbols to display (e.g., '(pass fail)).")

(defvar-local ert-flow--panel-name-regexp nil
  "If non-nil, only tests whose names match this regexp are displayed.")

(defvar-local ert-flow--panel-tags-filter nil
  "If non-nil, a list of tag strings; only tests having any of these tags are displayed.")

(defvar-local ert-flow--restore-point-suite nil
  "If non-nil, name of the suite to move point to after rendering.")

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
          (ert-flow--log "session: created root=%s panel=%s" abs (ert-flow--session-panel-buf-name sess))
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
    (when (ert-flow--get-watch-enabled sess)
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
(defun ert-flow--list-sessions-insert-header ()
  "Insert header for sessions list buffer."
  (insert (propertize "ert-flow sessions\n\n" 'face 'bold)))

(defun ert-flow--list-sessions-insert-row (s)
  "Insert a single row for session S in the sessions list."
  (let* ((root (ert-flow--session-root s))
         (panel (ert-flow--session-panel-buf-name s))
         (watch (if (ert-flow--get-watch-enabled s) "On" "Off"))
         (runner (ert-flow--conf s 'runner ert-flow-runner))
         (watch-mode (ert-flow--conf s 'watch-mode ert-flow-watch-mode))
         (proc (and (process-live-p (ert-flow--session-process s)) "active")))
    (insert (propertize (format "- %s\n" panel) 'face 'bold))
    (insert (format "  Runner: %s | Mode: %s | Watch: %s | Proc: %s\n"
                    (if (eq runner 'in-emacs-ert) "in-emacs" "external")
                    watch-mode watch (or proc "idle")))
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
        (ert-flow--list-sessions-insert-header)
        (if (= (hash-table-count ert-flow--sessions) 0)
            (insert "No active sessions.\n")
          (maphash (lambda (_root s) (ert-flow--list-sessions-insert-row s))
                   ert-flow--sessions))))
    (display-buffer buf)))

;;;###autoload
(defun ert-flow--dashboard-insert-header ()
  "Insert dashboard header section."
  (insert (propertize "ert-flow dashboard\n\n" 'face 'bold))
  (insert (format "Processes: active %d, queued %d\n"
                  ert-flow--active-run-count
                  (length ert-flow--run-queue)))
  (insert (format "Sessions: %d\n\n" (hash-table-count ert-flow--sessions))))

(defun ert-flow--dashboard-insert-session-row (s)
  "Insert a single dashboard row for session S."
  (let* ((root (ert-flow--session-root s))
         (panel (ert-flow--session-panel-buf-name s))
         (sess-name (file-name-nondirectory (directory-file-name root)))
         (watch-on (ert-flow--get-watch-enabled s))
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
        (ert-flow--dashboard-insert-header)
        (if (= (hash-table-count ert-flow--sessions) 0)
            (insert "No active sessions.\n")
          (maphash (lambda (_root s) (ert-flow--dashboard-insert-session-row s))
                   ert-flow--sessions))))
    (display-buffer buf)))

;;;; Watcher

(defun ert-flow--project-root ()
  "Return current project root directory as a string, or `default-directory'."
  (let* ((proj (project-current nil default-directory))
         (res (if proj
                  (expand-file-name (project-root proj))
                (expand-file-name default-directory))))
    (ert-flow--log "project-root: %s (proj=%s)" res (and proj (project-root proj)))
    res))

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
                   (ert-flow--get-watch-enabled sess))
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
  "Toggle automatic test running (watch) for the current session (panel-aware)."
  (interactive)
  (let* ((sess (or (and (eq major-mode 'ert-flow-panel-mode)
                        (ignore-errors (ert-flow--find-panel-session)))
                   (ert-flow--get-session (ert-flow--project-root))))
         (root (ert-flow--session-root sess))
         (old (ert-flow--get-watch-enabled sess))
         (new (not old)))
    (ert-flow--log "toggle-watch: panel=%s root=%s old=%s -> new=%s mode=%s"
                   (buffer-name) root (if old "on" "off") (if new "on" "off")
                   (ert-flow--conf sess 'watch-mode ert-flow-watch-mode))
    (ert-flow--set-watch-enabled sess new)
    (if new
        (progn
          (ert-flow--enable-watch sess)
          (message "ert-flow: watch enabled (%s)"
                   (ert-flow--conf sess 'watch-mode ert-flow-watch-mode)))
      (ert-flow--disable-watch sess)
      (message "ert-flow: watch disabled"))
    ;; Re-render this session's panel buffer
    (let ((bufname (ert-flow--session-panel-name root)))
      (when (get-buffer bufname)
        (with-current-buffer bufname
          (let ((ert-flow--panel-buffer-name bufname))
            (ert-flow--render)))))
    ;; Refresh header-line caches and visuals
    (when (fboundp 'ert-flow-headerline-refresh)
      (ert-flow-headerline-refresh))
    (force-mode-line-update t)
    (ert-flow--log "toggle-watch: updated UI for root=%s header+panel refreshed" root)))

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
(defun ert-flow--copy--gather-context ()
  "Collect context for building the copy string. Return plist."
  (let* ((root (ert-flow--project-root))
         (sess (ert-flow--get-session root))
         (results (or (and sess (ert-flow--get-last-results sess)) ert-flow--last-results))
         (sum (or (and sess (ert-flow--get-last-summary sess)) ert-flow--last-summary))
         (raw (or (and sess (ert-flow--get-last-raw-output sess)) ert-flow--last-raw-output))
         (fails (seq-filter (lambda (r) (memq (plist-get r :status) '(fail error))) (or results '())))
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
    (list :root root :sess sess :results results :sum sum :raw raw
          :fails fails :ts ts :proj proj :runner runner :cmd-str cmd-str :dur-str dur-str)))

(defun ert-flow--copy--build-string (ctx)
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
        (concat header body "\n" details (or stdout-block "") (or stderr-block ""))))))

(defun ert-flow-copy-failures ()
  "Copy failures/errors from the last run into the kill-ring (with backtraces).

Respects:
- `ert-flow-copy-format' — 'plain (default) | 'org | 'markdown
- `ert-flow-copy-backtrace-limit' — truncate details if set
- `ert-flow-copy-include-stdout' — include raw stdout tail"
  (interactive)
  (let* ((ctx (ert-flow--copy--gather-context))
         (fails (plist-get ctx :fails))
         (s (ert-flow--copy--build-string ctx)))
    (kill-new s)
    (message (if fails "ert-flow: failures copied" "ert-flow: no failures"))))

;;;###autoload
(defun ert-flow-clear ()
  "Clear panel and last results for the current session."
  (interactive)
  (let* ((sess (ert-flow--get-session (ert-flow--project-root))))
    (when sess
      (ert-flow--set-last-summary sess nil)
      (ert-flow--set-last-results sess nil)
      (ert-flow--set-last-raw-output sess nil)))
  ;; keep legacy globals in sync
  (setq ert-flow--last-summary nil
        ert-flow--last-results nil
        ert-flow--last-raw-output nil)
  (let* ((bufname (ert-flow--session-panel-name (ert-flow--project-root))))
    (when (get-buffer bufname)
      (let ((ert-flow--panel-buffer-name bufname))
        (ert-flow--render))))
  (message "ert-flow: cleared"))

;;;###autoload
(defun ert-flow-restart ()
  "Fully restart ert-flow after code reload.

Stops all sessions and processes, clears queues and counters, cancels timers,
and re-opens the panel for the current project."
  (interactive)
  (ert-flow--log "restart: begin")
  (condition-case err
      (progn
        (let ((cur-root (ert-flow--project-root)))
          ;; Kill all sessions (stops processes, watchers, cancels per-session timers)
          (ignore-errors (ert-flow-kill-all-sessions))
          ;; Cancel global timers
          (when (timerp ert-flow--idle-gc-timer)
            (cancel-timer ert-flow--idle-gc-timer))
          (setq ert-flow--idle-gc-timer nil)
          ;; Ensure after-save hook is detached
          (when (member #'ert-flow--after-save-hook after-save-hook)
            (remove-hook 'after-save-hook #'ert-flow--after-save-hook))
          (setq ert-flow--active-after-save-count 0)
          ;; Reset concurrency
          (setq ert-flow--run-queue nil
                ert-flow--active-run-count 0)
          ;; Reset legacy globals
          (setq ert-flow--last-raw-output nil
                ert-flow--last-results nil
                ert-flow--last-summary nil
                ert-flow--process nil)
          ;; Re-open panel for current project (auto-detect/first-run logic will apply)
          (let ((default-directory cur-root))
            (ert-flow--log "restart: reopen-panel root=%s" cur-root)
            (ert-flow-open-panel))
          (message "ert-flow: restarted")))
    (error
     (ert-flow--log "restart error: %S" err)
     (user-error "ert-flow: restart failed: %S" err))))

;;;; Minor mode

;;;###autoload
(defun ert-flow-toggle-logging ()
  "Toggle ert-flow logging and report the new state."
  (interactive)
  (setq ert-flow-log-enabled (not ert-flow-log-enabled))
  (message "ert-flow: logging %s" (if ert-flow-log-enabled "enabled" "disabled")))

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
