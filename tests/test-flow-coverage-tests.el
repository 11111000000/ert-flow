;;; test-flow-coverage-tests.el --- Tests for test-flow-coverage -*- lexical-binding: t; -*-

(require 'ert)
(require 'test-flow)
(require 'test-flow-coverage)

(defun test-flow-coverage-tests--lcov-sample ()
  "Return a minimal LCOV sample text."
  (let* ((file (expand-file-name "lisp/foo.el" default-directory))
         (sf (concat "SF:" file)))
    (mapconcat
     #'identity
     (list "TN:" sf "DA:1,1" "DA:2,0" "DA:3,2" "end_of_record" "")
     "\n")))

(ert-deftest test-flow-coverage/parse-basic ()
  (let* ((txt (test-flow-coverage-tests--lcov-sample))
         (lines (split-string txt "\n" t))
         (pair (test-flow-coverage--parse-lcov-lines lines))
         (sum (car pair))
         (files (cdr pair)))
    (should sum)
    (should files)
    (let ((lf (alist-get 'lines-found sum))
          (lh (alist-get 'lines-hit sum))
          (pct (alist-get 'percent sum)))
      (should (= lf 3))
      (should (= lh 2))
      (should (> pct 0.0)))
    (let* ((one (car files))
           (meta (cdr one)))
      (should (= (plist-get meta :lines-found) 3))
      (should (= (plist-get meta :lines-hit) 2))
      (should (equal (sort (copy-sequence (plist-get meta :missed-lines)) #'<) '(2))))))

(ert-deftest test-flow-coverage/overlays-apply ()
  (let* ((root (make-temp-file "efcov-root" t))
         (file (expand-file-name "foo.el" (expand-file-name "lisp" root)))
         (sess (test-flow--get-session root)))
    (unwind-protect
        (progn
          (make-directory (file-name-directory file) t)
          (with-temp-file file
            (insert "line1\nline2\nline3\n"))
          ;; fake coverage meta: line 2 missed
          (let* ((canon (test-flow-coverage--canon file))
                 (meta (list :lines-found 3 :lines-hit 2 :percent 66.6
                             :missed-lines (list 2)
                             :hits (make-hash-table :test 'eql))))
            (test-flow-coverage--sess-set sess 'coverage-summary '((lines-found . 3) (lines-hit . 2) (percent . 66.6)))
            (test-flow-coverage--sess-set sess 'coverage-files (list (cons canon meta))))
          (let ((default-directory root))
            (find-file file)
            (unwind-protect
                (progn
                  (test-flow-coverage-overlays-apply)
                  (should (= (length test-flow-coverage--overlays) 1)))
              (kill-buffer (current-buffer)))))
      (ignore-errors (delete-directory root t)))))

(ert-deftest test-flow-coverage/insert-block-smoke ()
  (let* ((root (test-flow--project-root))
         (sess (test-flow--get-session root))
         (bufname (test-flow--session-panel-name root))
         (canon (test-flow-coverage--canon (expand-file-name "lisp/foo.el" root)))
         (meta (list :lines-found 3 :lines-hit 2 :percent 66.6 :missed-lines (list 2) :hits (make-hash-table :test 'eql))))
    (test-flow-coverage--sess-set sess 'coverage-summary '((lines-found . 3) (lines-hit . 2) (percent . 66.6)))
    (test-flow-coverage--sess-set sess 'coverage-files (list (cons canon meta)))
    (with-current-buffer (get-buffer-create bufname)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (test-flow-coverage--insert-panel-block sess)
        (let ((s (buffer-string)))
          (should (string-match-p "Coverage" s))
          (should (string-match-p "Total" s)))))))

(provide 'test-flow-coverage-tests)
;;; test-flow-coverage-tests.el ends here
