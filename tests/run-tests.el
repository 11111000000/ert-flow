;;; run-tests.el --- Batch runner for test-flow tests -*- lexical-binding: t; -*-

;; Usage:
;;   emacs -Q --batch -l tests/run-tests.el

(let* ((this (or load-file-name (buffer-file-name)))
       (root (expand-file-name ".." (file-name-directory this)))
       (lisp (expand-file-name "lisp" root)))
  (add-to-list 'load-path lisp)
  (add-to-list 'load-path root)
  (require 'test-flow)
  ;; Optional: coverage module and its tests
  (ignore-errors (require 'test-flow-coverage))
  (load (expand-file-name "tests/test-flow-tests.el" root) nil t)
  (let ((cov-tests (expand-file-name "tests/test-flow-coverage-tests.el" root)))
    (when (file-exists-p cov-tests)
      (load cov-tests nil t))))

(ert-run-tests-batch-and-exit)

;;; run-tests.el ends here
