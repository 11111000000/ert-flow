;;; test-flow-parse.el --- Parsing for test-flow (JSON/ERT-batch) -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Self-contained parsing layer:
;; - test-flow-parse-batch-output: parse ERT batch output
;; - test-flow-parse-json-output:  parse JSON payload
;; - test-flow-parse-output:       dispatch by `test-flow-parser' ('auto|'json|'ert-batch)
;;
;; Does not depend on UI/runner. It calls core log if available.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'json)

;;;###autoload
(defgroup test-flow-parse nil
  "Parsing options for test-flow."
  :group 'test-flow)

(defun test-flow-parse--log (fmt &rest args)
  "Log via core logger if available."
  (when (fboundp 'test-flow--log)
    (apply 'test-flow--log fmt args)))

;;;; Batch (ERT) parsing

(defun test-flow-parse--batch-clean-name (s)
  "Return NAME from ERT summary line, stripping timing/location suffixes."
  (let* ((nm (string-trim (or s ""))))
    (if (string-match "\\`\\([^ ]+\\)" nm)
        (match-string 1 nm)
      nm)))

(defun test-flow-parse--batch--extract-details-block (lines i)
  "Extract details block starting after index I in LINES.
Return cons (block . next-i)."
  (let* ((start (1+ i))
         (j start))
    (while (and (< j (length lines))
                (not (string-match "^Test[ \t]+" (nth j lines))))
      (cl-incf j))
    (cons (string-join (cl-subseq lines start j) "\n") (1- j))))

(defun test-flow-parse--suite-of (test-name)
  "Local copy to avoid hard dependency on core."
  (let* ((s (or test-name ""))
         (idx (string-match "/" s)))
    (if (and idx (> idx 0))
        (substring s 0 idx)
      s)))

(defun test-flow-parse--batch-pass1 (lines)
  "Scan LINES and collect name->status/details, totals and time."
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
     (when (string-match "^[ \t]*passed[ \t]+[0-9]+/[0-9]+[ \t]+\\([^ \t]+\\)" line)
       (let ((nm (match-string 1 line)))
         (puthash nm t all-names)
         (puthash nm 'pass name->status)))
     (when (string-match "^[ \t]*\\(FAILED\\|ERROR\\|SKIPPED\\|XFAIL\\|XPASS\\)[ \t]+[0-9]+/[0-9]+[ \t]+\\(.+\\)$" line)
       (let* ((kw (match-string 1 line))
              (nm (test-flow-parse--batch-clean-name (match-string 2 line)))
              (st (pcase kw
                    ("FAILED"  'fail)
                    ("ERROR"   'error)
                    ("SKIPPED" 'skip)
                    ("XFAIL"   'xfail)
                    ("XPASS"   'fail)
                    (_ 'fail))))
         (puthash nm t all-names)
         (puthash nm st name->status)))
     (when (string-match "^Test[ \t]+\\([^ \t]+\\)[ \t]+\\(backtrace\\|condition\\):$" line)
       (let* ((nm (match-string 1 line))
              (pair (test-flow-parse--batch--extract-details-block lines i))
              (block (car pair))
              (next-i (cdr pair)))
         (puthash nm t all-names)
         (puthash nm (string-trim block) name->details)
         (setq i next-i)))
     (when (and (null total)
                (string-match "^Ran[ \t]+\\([0-9]+\\)[ \t]+tests?" line))
       (setq total (string-to-number (match-string 1 line))))
     (when (and (null unexpected)
                (string-match "^\\([0-9]+\\)[ \t]+unexpected results?:" line))
       (setq unexpected (string-to-number (match-string 1 line))))
     (when (and (null unexpected)
                (string-match "^[ \t]*Ran[ \t]+[0-9]+[ \t]+tests?,[ \t]+\\([0-9]+\\)[ \t]+results? were unexpected\\." line))
       (setq unexpected (string-to-number (match-string 1 line))))
     (when (and (null time-str)
                (string-match "in[ \t]+\\([0-9.]+\\)[ \t]+seconds" line))
       (setq time-str (match-string 1 line)))
     (cl-incf i))
    (list :name->status name->status
          :name->details name->details
          :all-names all-names
          :total total :unexpected unexpected :time time-str)))

(defun test-flow-parse--batch-pass2-final-status (lines name->status all-names)
  "Update NAME->STATUS and ALL-NAMES from final section in LINES."
  (dolist (line lines)
    (when (string-match "^[ \t]*\\(FAILED\\|ERROR\\|SKIPPED\\|XFAIL\\|XPASS\\)\\(?:[ \t]+[0-9]+/[0-9]+\\)?[ \t]+\\(.+\\)$" line)
      (let* ((kw (match-string 1 line))
             (nm (test-flow-parse--batch-clean-name (match-string 2 line)))
             (st (pcase kw
                   ("FAILED"  'fail)
                   ("ERROR"   'error)
                   ("SKIPPED" 'skip)
                   ("XFAIL"   'xfail)
                   ("XPASS"   'fail)
                   (_ 'fail))))
        (puthash nm t all-names)
        (puthash nm st name->status)))))

(defun test-flow-parse--batch-build-results (tables)
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
              (suite (test-flow-parse--suite-of nm)))
         (push (list :name nm
                     :status status
                     :message (or message (symbol-name status))
                     :details details
                     :suite suite)
               results)))
     all-names)
    (nreverse results)))

(defun test-flow-parse--batch-build-summary (time-str total unexpected results)
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

;;;###autoload
(defun test-flow-parse-batch-output (out)
  "Parse ERT batch OUT and return (SUMMARY . RESULTS)."
  (let* ((lines (split-string (or out "") "\n"))
         (t1 (test-flow-parse--batch-pass1 lines))
         (_ (test-flow-parse--batch-pass2-final-status
             lines
             (plist-get t1 :name->status)
             (plist-get t1 :all-names)))
         (results (test-flow-parse--batch-build-results t1))
         (summary (test-flow-parse--batch-build-summary
                   (plist-get t1 :time)
                   (plist-get t1 :total)
                   (plist-get t1 :unexpected)
                   results)))
    (let* ((p (cl-count-if (lambda (r) (eq (plist-get r :status) 'pass)) results))
           (f (cl-count-if (lambda (r) (eq (plist-get r :status) 'fail)) results))
           (e (cl-count-if (lambda (r) (eq (plist-get r :status) 'error)) results))
           (s (cl-count-if (lambda (r) (memq (plist-get r :status) '(skip xfail))) results))
           (u (and (listp summary) (alist-get 'unexpected summary))))
      (test-flow-parse--log "batch parsed: total=%s results=%d P:%d F:%d E:%d S:%d U:%s time=%s"
                            (or (alist-get 'total summary) "?")
                            (length results)
                            p f e s
                            (or u 0)
                            (or (alist-get 'time summary) "-")))
    (when (and (= (length results) 0)
               (stringp out) (> (length out) 0))
      (test-flow-parse--log "batch parsed 0 results; first 200 chars:\n%s"
                            (substring out 0 (min 200 (length out)))))
    (cons summary results)))

;;;; JSON parsing and auto-dispatch

(defun test-flow-parse--json-safe-substring (s)
  "Return substring of S from first { to last }, or nil."
  (when (stringp s)
    (let ((start (string-match "{" s)))
      (when start
        (let ((end (cl-position ?} s :from-end t)))
          (when (and end (>= end start))
            (substring s start (1+ end))))))))

(defun test-flow-parse--aget (key obj)
  "Lookup KEY in alist OBJ, tolerating string or symbol keys."
  (or (alist-get key obj nil nil #'equal)
      (and (stringp key) (alist-get (intern key) obj))
      (and (symbolp key) (alist-get (symbol-name key) obj nil nil #'equal))))

(defun test-flow-parse--json-parse (out)
  "Return parsed JSON object (alist) from OUT or signal on failure."
  (let* ((json-str (or (test-flow-parse--json-safe-substring out) out)))
    (json-parse-string json-str :object-type 'alist :array-type 'list)))

(defun test-flow-parse--json-tests (obj)
  "Extract tests array from OBJ, returning a list or nil."
  (let ((tests-raw (test-flow-parse--aget "tests" obj)))
    (cond
     ((null tests-raw) nil)
     ((listp tests-raw) tests-raw)
     ((vectorp tests-raw) (append tests-raw nil))
     (t (signal 'wrong-type-argument (list 'list tests-raw))))))

(defun test-flow-parse--json-test->plist (tobj)
  "Convert a single TOBJ (alist from JSON) into a result plist."
  (let* ((nm (test-flow-parse--aget "name" tobj))
         (st-str (test-flow-parse--aget "status" tobj))
         (st (pcase (and st-str (downcase (format "%s" st-str)))
               ("pass" 'pass) ("ok" 'pass)
               ("fail" 'fail) ("failed" 'fail)
               ("error" 'error)
               ("skip" 'skip) ("skipped" 'skip)
               ("xfail" 'xfail)
               (_ 'fail)))
         (msg (test-flow-parse--aget "message" tobj))
         (det (test-flow-parse--aget "details" tobj))
         (file (test-flow-parse--aget "file" tobj))
         (line (test-flow-parse--aget "line" tobj))
         (suite (test-flow-parse--suite-of nm)))
    (list :name (and nm (format "%s" nm))
          :status st
          :message (or msg (and det (car (split-string (format "%s" det) "\n" t))) (symbol-name st))
          :details (or (and det (format "%s" det)) "")
          :suite suite
          :file (and file (format "%s" file))
          :line (and line (string-to-number (format "%s" line))))))

(defun test-flow-parse--json-build-summary (summary-raw results)
  "Build summary alist from SUMMARY-RAW (alist) and RESULTS (list)."
  (let* ((raw-total (and summary-raw (test-flow-parse--aget "total" summary-raw)))
         (total (cond
                 ((numberp raw-total) raw-total)
                 ((and (stringp raw-total) (string-match-p "\\`[0-9]+\\'" raw-total))
                  (string-to-number raw-total))
                 (t (length results))))
         (p (or (and summary-raw (test-flow-parse--aget "passed" summary-raw))
                (cl-count-if (lambda (r) (eq (plist-get r :status) 'pass)) results)))
         (f (or (and summary-raw (test-flow-parse--aget "failed" summary-raw))
                (cl-count-if (lambda (r) (eq (plist-get r :status) 'fail)) results)))
         (e (or (and summary-raw (test-flow-parse--aget "error" summary-raw))
                (cl-count-if (lambda (r) (eq (plist-get r :status) 'error)) results)))
         (s (or (and summary-raw (test-flow-parse--aget "skipped" summary-raw))
                (cl-count-if (lambda (r) (memq (plist-get r :status) '(skip xfail))) results)))
         (unexpected (seq-count (lambda (r) (memq (plist-get r :status) '(fail error))) results))
         (duration-ms
          (let ((dm (and summary-raw (test-flow-parse--aget "duration_ms" summary-raw)))
                (tstr (and summary-raw (test-flow-parse--aget "time" summary-raw))))
            (cond
             ((numberp dm) dm)
             ((and (stringp dm) (string-match-p "\\`[0-9]+\\'" dm)) (string-to-number dm))
             ((and (stringp tstr) (string-match-p "\\`[0-9.]+\\'" tstr))
              (truncate (* 1000 (string-to-number tstr))))
             (t nil))))
         (time-str (let ((tval (and summary-raw (test-flow-parse--aget "time" summary-raw))))
                     (and tval (format "%s" tval)))))
    `((total . ,total) (unexpected . ,unexpected)
      (time . ,time-str) (duration-ms . ,duration-ms)
      (passed . ,p) (failed . ,f) (error . ,e) (skipped . ,s))))

;;;###autoload
(defun test-flow-parse-json-output (out)
  "Parse JSON OUT and return (SUMMARY . RESULTS) or nil on failure."
  (condition-case _err
      (let* ((obj (test-flow-parse--json-parse out))
             (tests-raw (test-flow-parse--json-tests obj))
             (results (mapcar #'test-flow-parse--json-test->plist tests-raw))
             (summary-raw (test-flow-parse--aget "summary" obj))
             (summary (test-flow-parse--json-build-summary summary-raw results)))
        (cons summary results))
    (error nil)))

;;;###autoload
(defun test-flow-parse-output (out)
  "Dispatch parsing according to `test-flow-parser' (or 'auto if unbound)."
  (let ((mode (if (boundp 'test-flow-parser) test-flow-parser 'auto)))
    (pcase mode
      ('json (or (test-flow-parse-json-output out)
                 (cons '((total . 0) (unexpected . 0) (time . nil)) '())))
      ('ert-batch (test-flow-parse-batch-output out))
      ('auto (or (test-flow-parse-json-output out)
                 (test-flow-parse-batch-output out)))
      (_ (test-flow-parse-batch-output out)))))

;;; Compatibility shims (monolith-era names)
;; Keep older internal names working for tests and third-party configs.

;;;###autoload
(unless (fboundp 'test-flow--parse-batch-output)
  (defun test-flow--parse-batch-output (out)
    "Compatibility shim: parse ERT batch OUT via test-flow-parse."
    (test-flow-parse-batch-output out)))

;;;###autoload
(unless (fboundp 'test-flow--parse-json-output)
  (defun test-flow--parse-json-output (out)
    "Compatibility shim: parse JSON OUT via test-flow-parse."
    (test-flow-parse-json-output out)))

;;;###autoload
(unless (fboundp 'test-flow--parse-output)
  (defun test-flow--parse-output (out)
    "Compatibility shim: dispatch parsing according to `test-flow-parser'."
    (test-flow-parse-output out)))

(provide 'test-flow-parse)
;;; test-flow-parse.el ends here
