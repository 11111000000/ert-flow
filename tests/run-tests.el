;;; run-tests.el --- Batch runner for ert-flow tests -*- lexical-binding: t; -*-

;; Usage:
;;   emacs -Q --batch -l tests/run-tests.el

(let* ((this (or load-file-name (buffer-file-name)))
       (root (expand-file-name ".." (file-name-directory this))))
  (add-to-list 'load-path root)
  (load (expand-file-name "ert-flow.el" root) nil t)
  (load (expand-file-name "tests/ert-flow-tests.el" root) nil t))

(ert-run-tests-batch-and-exit)

;;; run-tests.el ends here
