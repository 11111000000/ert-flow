;;; coverage.el --- Generate Elisp coverage via undercover → lcov.info  -*- lexical-binding: t; -*-

;; Usage:
;;   nix run .#coverage
;;
;; Steps:
;;   1) Instrument l/*.el with undercover (local file report, no upload)
;;   2) Load and run ERT tests (batch, without -and-exit)
;;   3) Save Coveralls JSON, convert to LCOV at coverage/lcov.info
;;   4) Exit with non-zero code if tests had unexpected results

(require 'cl-lib)
(require 'subr-x)
(require 'json)
(require 'seq)

(defun efcov--find-root (start)
  "Ascend from START to find project root by markers: flake.nix, Cask, .git, lisp/, tests/."
  (let ((dir (file-name-as-directory (expand-file-name start)))
        (prev nil)
        (res nil))
    (while (and (not res) (not (equal dir prev)))
      (when (or (file-exists-p (expand-file-name "flake.nix" dir))
                (file-exists-p (expand-file-name "Cask" dir))
                (file-directory-p (expand-file-name ".git" dir))
                (file-directory-p (expand-file-name "lisp" dir))
                (file-directory-p (expand-file-name "tests" dir)))
        (setq res dir))
      (setq prev dir
            dir (file-name-directory (directory-file-name dir))))
    (or res (file-name-as-directory (expand-file-name start)))))

(let* ((root (or (ignore-errors (efcov--find-root default-directory))
                 (file-name-as-directory (expand-file-name default-directory))))
       ;; Ensure all subsequent paths resolve under the repo root
       (default-directory root)
       ;; Prefer conventional dirs; fallback to short aliases.
       (lisp (or (and (file-directory-p (expand-file-name "lisp" root))
                      (expand-file-name "lisp" root))
                 (expand-file-name "l" root)))
       (tests (or (and (file-directory-p (expand-file-name "tests" root))
                       (expand-file-name "tests" root))
                  (and (file-directory-p (expand-file-name "test" root))
                       (expand-file-name "test" root))
                  (expand-file-name "t" root)))
       (cov-dir (expand-file-name "coverage" root))
       (coveralls-json (expand-file-name "coveralls.json" cov-dir))
       (lcov-info (expand-file-name "lcov.info" cov-dir)))
  (make-directory cov-dir t)
  (add-to-list 'load-path lisp)

  ;; 1) undercover instrumentation before loading project files (optional)
  (let ((have-undercover (ignore-errors (require 'undercover))))
    (message "undercover: loaded? %s" (if have-undercover "t" "nil"))
    (when have-undercover
      ;; Undercover — макрос, ожидает литеральные строки, а не выражения.
      ;; Дадим сразу оба паттерна: для lisp/ и для короткого l/.
      (undercover "lisp/*.el" "l/*.el"
                  (:report-file coveralls-json)
                  (:send-report nil))))

  ;; 2) Load and run tests (avoid *-and-exit to continue post-processing)
  (require 'ert)
  (load (expand-file-name "test-flow.el" lisp) nil t)
  ;; Core tests
  (load (expand-file-name "test-flow-tests.el" tests) nil t)
  ;; Coverage tests (optional)
  (let ((cov-tests (expand-file-name "test-flow-coverage-tests.el" tests)))
    (when (file-exists-p cov-tests)
      (load cov-tests nil t)))

  ;; Run all tests; return number of unexpected results (fail/error)
  (let* ((unexpected (ert-run-tests-batch t)))

    ;; 3) Ensure Undercover writes report (older/newer function names)
    (cond
     ((fboundp 'undercover-save-report) (undercover-save-report))
     ((fboundp 'undercover-report) (undercover-report))
     (t (message "undercover: no explicit save-report function; relying on defaults")))
    (message "undercover: expected report at %s" coveralls-json)

    ;; Convert Coveralls JSON → LCOV with robust fallback.
    (let* ((jpath
            (cond
             ((file-exists-p coveralls-json) coveralls-json)
             (t
              ;; Расширенный поиск: берём первый JSON, содержащий "source_files"
              (let* ((cands (ignore-errors
                              (append
                               (directory-files-recursively cov-dir "\\.json\\'")
                               (directory-files-recursively root "\\`coveralls\\.json\\'"))))
                     (hit (cl-find-if
                           (lambda (p)
                             (ignore-errors
                               (with-temp-buffer
                                 (insert-file-contents p)
                                 (search-forward "\"source_files\"" nil t))))
                           cands)))
                hit)))))
      (if (and jpath (file-exists-p jpath))
          (progn
            (message "coverage: converting %s → %s" jpath lcov-info)
            (with-temp-buffer
              (insert-file-contents jpath)
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
                        ;; Optional LF/LH (helps other tools)
                        (insert (format "LF:%d\nLH:%d\n" lf lh)))
                      (insert "end_of_record\n"))))))
            (message "coverage: wrote %s" lcov-info))
        ;; Fallback: создаём минимальный LCOV (без DA), чтобы файл точно был.
        (message "coverage: coveralls.json not found; writing minimal LCOV to %s" lcov-info)
        (let* ((cands (list lisp (expand-file-name "l" root)))
               (src-dirs (cl-remove-if-not #'file-directory-p cands))
               (els (apply #'append
                           (mapcar (lambda (d)
                                     (ignore-errors
                                       (directory-files-recursively d "\\.el\\'")))
                                   src-dirs))))
          (with-temp-file lcov-info
            (dolist (p els)
              (insert (format "TN:\nSF:%s\nend_of_record\n" (expand-file-name p root))))))
        (message "coverage: wrote minimal %s" lcov-info)))

    ;; 4) Exit code: 0 if OK, 1 if unexpected results
    (message "coverage: lcov at %s exists? %s"
             lcov-info (if (file-exists-p lcov-info) "yes" "no"))
    (kill-emacs (if (and (numberp unexpected) (> unexpected 0)) 1 0))))
