;;; coverage.el --- Run ERT with Undercover LCOV  -*- lexical-binding: t; -*-

;; Usage:
;;   make coverage
;;   nix run .#coverage

(let* ((default-directory (file-name-directory (or load-file-name buffer-file-name)))
       (root (expand-file-name ".." default-directory)))
  ;; Make sure project lisp/ is on load-path
  (add-to-list 'load-path (expand-file-name "lisp" root)))

(require 'undercover)

;; Force coverage if not enabled via env
(unless (getenv "UNDERCOVER_FORCE")
  (setq undercover-force-coverage t))

;; Configure LCOV output
(undercover
 "lisp/*.el"
 (:exclude "tests/*.el" "t/*.el")
 (:report-format 'lcov)
 (:report-file "coverage/lcov.info")
 (:send-report nil)
 (:merge-report nil)
 (:verbosity 6))

;; Find test entrypoint
(let* ((root (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name))))
       (cand1 (expand-file-name "tests/run-tests.el" root))
       (cand2 (expand-file-name "t/run-tests.el" root))
       (target (cond
                ((file-exists-p cand1) cand1)
                ((file-exists-p cand2) cand2)
                (t (error "No run-tests.el found under tests/ or t/")))))
  (load-file target))

;; If the test runner didn't exit, make sure we report before batch exit.
(when noninteractive
  (undercover-report 'lcov))
