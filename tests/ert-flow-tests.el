;;; ert-flow-tests.el --- Tests for ert-flow -*- lexical-binding: t; -*-

(require 'ert)
(require 'ert-flow)

(defun ert-flow-tests--sample-batch ()
  "Return a sample ERT batch output string."
  (mapconcat
   #'identity
   '("Running 2 tests"
     "Test ns/ok passed."
     "Test ns/f1 backtrace:"
     "  Assertion failed: (= 1 2)"
     "  Backtrace: ..."
     "Ran 2 tests, 1 results were unexpected."
     "FAILED ns/f1"
     "")
   "\n"))

(defun ert-flow-tests--sample-json ()
  "Return a sample JSON output string."
  (concat
   "noise before...\n"
   (json-encode
    `(("summary" . (("total" . 2) ("failed" . 1) ("error" . 0) ("duration_ms" . 12)))
      ("tests" .
       [ (("name" . "ns/ok") ("status" . "pass"))
         (("name" . "ns/f1") ("status" . "fail") ("message" . "boom") ("details" . "Assertion failed")) ])))
   "\ntrailing noise"))

(ert-deftest ert-flow/suite-of ()
  (should (equal (ert-flow--suite-of "acp/parser/x") "acp"))
  (should (equal (ert-flow--suite-of "single") "single"))
  (should (equal (ert-flow--suite-of "") "")))

(ert-deftest ert-flow/normalize-command ()
  (should (equal (ert-flow--normalize-command '("emacs" "-Q"))
                 '("emacs" "-Q")))
  (let ((cmd (ert-flow--normalize-command "echo ok")))
    (should (listp cmd))
    (should (stringp (car cmd)))
    (should (member "-lc" cmd))))

(ert-deftest ert-flow/parse-batch-output ()
  (let* ((out (ert-flow-tests--sample-batch))
         (parsed (ert-flow--parse-batch-output out))
         (summary (car parsed))
         (results (cdr parsed)))
    (should (equal (alist-get 'total summary) 2))
    (should (equal (alist-get 'unexpected summary) 1))
    (should (= (length results) 2))
    (let* ((r-ok (seq-find (lambda (r) (equal (plist-get r :name) "ns/ok")) results))
           (r-f1 (seq-find (lambda (r) (equal (plist-get r :name) "ns/f1")) results)))
      (should (eq (plist-get r-ok :status) 'pass))
      (should (memq (plist-get r-f1 :status) '(fail error)))
      (should (string-match-p "Assertion failed" (or (plist-get r-f1 :details) ""))))))

(ert-deftest ert-flow/parse-json-output ()
  (let* ((out (ert-flow-tests--sample-json))
         (parsed (ert-flow--parse-json-output out)))
    (should parsed)
    (let* ((summary (car parsed))
           (results (cdr parsed)))
      (should (= (alist-get 'total summary) 2))
      (should (= (length results) 2))
      (let ((r-ok (seq-find (lambda (r) (equal (plist-get r :name) "ns/ok")) results))
            (r-f1 (seq-find (lambda (r) (equal (plist-get r :name) "ns/f1")) results)))
        (should (eq (plist-get r-ok :status) 'pass))
        (should (eq (plist-get r-f1 :status) 'fail))
        (should (equal (plist-get r-f1 :message) "boom"))))))

(ert-deftest ert-flow/parse-output-auto ()
  (let ((ert-flow-parser 'auto))
    (let ((json-out (ert-flow-tests--sample-json)))
      (should (ert-flow--parse-output json-out)))
    (let ((batch-out (ert-flow-tests--sample-batch)))
      (should (ert-flow--parse-output batch-out)))))

(ert-deftest ert-flow/file-event-eligible ()
  (let ((ert-flow-watch-include-regexp "\\.el\\'")
        (ert-flow-watch-exclude-regexp "/dist/"))
    (should (ert-flow--file-event-eligible-p "/x/a.el"))
    (should-not (ert-flow--file-event-eligible-p "/x/a.txt"))
    (should-not (ert-flow--file-event-eligible-p "/x/dist/a.el"))))

(ert-deftest ert-flow/collect-dirs-respects-depth-and-exclude ()
  (let* ((root (make-temp-file "ert-flow-root" t))
         (a (expand-file-name "a" root))
         (b (expand-file-name "a/b" root))
         (dist (expand-file-name "dist" root)))
    (unwind-protect
        (progn
          (make-directory a t)
          (make-directory b t)
          (make-directory dist t)
          (let ((ert-flow-watch-exclude-regexp "/dist/"))
            (let ((dirs (ert-flow--collect-dirs root 1)))
              (should (member root dirs))
              (should (member a dirs))
              (should-not (member b dirs))     ;; depth limit
              (should-not (member dist dirs))))) ;; excluded
      (ignore-errors (delete-directory root t)))))

(ert-deftest ert-flow/copy-failures ()
  (let ((ert-flow--last-results
         (list (list :name "ns/ok" :status 'pass :message "ok")
               (list :name "ns/f1" :status 'fail :message "boom" :details "D1\nD2")
               (list :name "ns/e1" :status 'error :message "kaboom" :details "E1"))))
    (let ((kill-ring nil))
      (ert-flow-copy-failures)
      (let ((s (current-kill 0 t)))
        (should (string-match-p "ns/f1: boom" s))
        (should (string-match-p "ns/e1: kaboom" s))
        (should (string-match-p "=== ns/f1 ===" s))))))

(provide 'ert-flow-tests)
;;; ert-flow-tests.el ends here
