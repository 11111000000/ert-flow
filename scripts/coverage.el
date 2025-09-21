;;; coverage.el --- Generate Elisp coverage via undercover → lcov.info  -*- lexical-binding: t; -*-

;; Usage:
;;   nix run .#coverage
;;
;; Steps:
;;   1) Instrument lisp/*.el with undercover (local file report, no upload)
;;   2) Load and run ERT tests (batch, without -and-exit)
;;   3) Save Coveralls JSON, convert to LCOV at coverage/lcov.info
;;   4) Exit with non-zero code if tests had unexpected results

(require 'cl-lib)
(require 'subr-x)
(require 'json)

(let* ((root (expand-file-name default-directory))
       (lisp (expand-file-name "lisp" root))
       (tests (expand-file-name "tests" root))
       (cov-dir (expand-file-name "coverage" root))
       (coveralls-json (expand-file-name "coveralls.json" cov-dir))
       (lcov-info (expand-file-name "lcov.info" cov-dir)))
  (make-directory cov-dir t)
  (add-to-list 'load-path lisp)

  ;; 1) undercover instrumentation before loading project files
  (require 'undercover)
  ;; Generate local report; do not send anywhere. Keep simple Coveralls JSON.
  (undercover "lisp/*.el"
              (:report-file coveralls-json)
              (:send-report nil))

  ;; 2) Load and run tests (avoid *-and-exit to continue post-processing)
  (require 'ert)
  (load (expand-file-name "ert-flow.el" lisp) nil t)
  ;; Core tests
  (load (expand-file-name "ert-flow-tests.el" tests) nil t)
  ;; Coverage tests (optional)
  (let ((cov-tests (expand-file-name "ert-flow-coverage-tests.el" tests)))
    (when (file-exists-p cov-tests)
      (load cov-tests nil t)))

  ;; Run all tests; return number of unexpected results (fail/error)
  (let* ((unexpected (ert-run-tests-batch t)))

    ;; 3) Ensure Undercover writes report (older/newer function names)
    (cond
     ((fboundp 'undercover-save-report) (undercover-save-report))
     ((fboundp 'undercover-report) (undercover-report))
     (t (message "undercover: no explicit save-report function; relying on defaults")))

    ;; Convert Coveralls JSON → LCOV if available.
    (when (file-exists-p coveralls-json)
      (message "coverage: converting %s → %s" coveralls-json lcov-info)
      (with-temp-buffer
        (insert-file-contents coveralls-json)
        (let* ((obj (json-parse-buffer :object-type 'alist :array-type 'list))
               (files (or (alist-get 'source_files obj) (alist-get "source_files" obj))))
          (with-temp-file lcov-info
            (dolist (f files)
              (let* ((name (or (alist-get 'name f) (alist-get "name" f)))
                     (cov  (or (alist-get 'coverage f) (alist-get "coverage" f)))
                     (abs  (expand-file-name name root)))
                (insert (format "TN:\nSF:%s\n" abs))
                (let ((line 0) (lf 0) (lh 0))
                  (dolist (hits cov)
                    (setq line (1+ line))
                    (when (numberp hits)
                      (setq lf (1+ lf))
                      (when (> hits 0) (setq lh (1+ lh)))
                      (insert (format "DA:%d,%d\n" line (truncate hits)))))
                  ;; Optional LF/LH (our parser tolerates absence, but adding helps other tools)
                  (insert (format "LF:%d\nLH:%d\n" lf lh)))
                (insert "end_of_record\n")))))))

    ;; 4) Exit code: 0 if OK, 1 if unexpected results
    (kill-emacs (if (and (numberp unexpected) (> unexpected 0)) 1 0))))
