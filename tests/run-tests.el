;;; run-tests.el --- Batch runner for ert-flow tests -*- lexical-binding: t; -*-

;; Usage:
;;   emacs -Q --batch -l tests/run-tests.el

(let* ((this (or load-file-name (buffer-file-name)))
       (root (expand-file-name ".." (file-name-directory this)))
       (lisp (expand-file-name "lisp" root)))
  (add-to-list 'load-path lisp)
  (add-to-list 'load-path root)
  (require 'ert-flow)
  (load (expand-file-name "tests/ert-flow-tests.el" root) nil t))

(ert-run-tests-batch-and-exit)

;;; run-tests.el ends here
