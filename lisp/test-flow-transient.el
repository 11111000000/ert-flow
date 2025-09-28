;;; test-flow-transient.el --- Global transient and keybindings for test-flow  -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: test-flow maintainers
;; Keywords: tools, convenience, testing
;; Package-Requires: ((emacs "27.1") (transient "0.3.7"))
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Provides:
;; - A global transient prefix `test-flow-transient' aggregating global actions.
;; - A global minor mode `test-flow-global-mode' that binds the transient to a
;;   customizable key (default: "C-c f").
;; - An additional key (default: "C-c f f") to toggle the Test Flow panel.
;;
;; Notes:
;; - Emacs convention: packages should avoid globally binding "C-c <letter>".
;;   This file follows your explicit requirement to use "C-c f" by default,
;;   but you can change it via customization.
;; - Labels/messages support RU/EN localization (basic). You can set
;;   `test-flow-language' to 'ru, 'en, or 'auto. In 'auto, we guess from
;;   `current-language-environment'.
;;
;; Setup:
;;   (require 'test-flow-transient)
;;   ;; Enabled by default if `test-flow-global-transient-enable' is non-nil.
;;
;; If transient is not installed, `test-flow-transient' will politely explain.

;;; Code:

(eval-when-compile (require 'subr-x))
(require 'test-flow-i18n)

(defgroup test-flow nil
  "Test Flow tools."
  :group 'tools)

;; i18n moved to test-flow-i18n.el

;; -----------------------------------------------------------------------------
;; Wrappers: call existing commands if available (safe no-ops otherwise)
;; -----------------------------------------------------------------------------

(defun test-flow--call-first-defined (candidates)
  "Call interactively the first function from CANDIDATES that is fboundp.
If none found, show localized message."
  (let (called)
    (dolist (fn candidates)
      (when (and (symbolp fn) (fboundp fn) (not called))
        (setq called t)
        (call-interactively fn)))
    (unless called
      (message "%s" (test-flow-i18n 'no-command)))))

;;;###autoload
(defun test-flow-cmd-run-all ()
  "Run all tests (wrapper)."
  (interactive)
  (test-flow--call-first-defined
   '(test-flow-run-all
     test-flow-tests-run-all
     test-flow-run
     run-tests)))

;;;###autoload
(defun test-flow-cmd-run-file ()
  "Run tests in current file (wrapper)."
  (interactive)
  (test-flow--call-first-defined
   '(test-flow-run-file
     test-flow-tests-run-file
     run-tests-file)))

;;;###autoload
(defun test-flow-cmd-run-at-point ()
  "Run test at point (wrapper)."
  (interactive)
  (test-flow--call-first-defined
   '(test-flow-run-at-point
     test-flow-tests-run-at-point
     run-test-at-point)))

;;;###autoload
(defun test-flow-cmd-rerun ()
  "Re-run last tests (wrapper)."
  (interactive)
  (test-flow--call-first-defined
   '(test-flow-rerun
     test-flow-tests-rerun
     rerun-tests)))

;;;###autoload
(defun test-flow-cmd-coverage-toggle ()
  "Toggle coverage overlay (wrapper)."
  (interactive)
  (test-flow--call-first-defined
   '(test-flow-coverage-toggle
     test-flow-coverage-mode
     coverage-toggle)))

;;;###autoload
(defun test-flow-cmd-coverage-show ()
  "Show coverage report (wrapper)."
  (interactive)
  (test-flow--call-first-defined
   '(test-flow-coverage-show
     coverage-show
     test-flow-open-coverage)))

;;;###autoload
(defun test-flow-cmd-coverage-refresh ()
  "Refresh coverage data (wrapper)."
  (interactive)
  (test-flow--call-first-defined
   '(test-flow-coverage-refresh
     coverage-refresh
     test-flow-coverage-update)))

;;;###autoload
(defun test-flow-cmd-open-report ()
  "Open test report (wrapper)."
  (interactive)
  (test-flow--call-first-defined
   '(test-flow-open-report
     test-flow-report-open
     open-test-report)))

;;;###autoload
(defun test-flow-cmd-open-last-log ()
  "Open last test log (wrapper)."
  (interactive)
  (test-flow--call-first-defined
   '(test-flow-open-last-log
     test-flow-log-open-last
     open-last-test-log)))

;;;###autoload
(defun test-flow-cmd-detect-runner ()
  "Detect and configure the appropriate test runner (wrapper)."
  (interactive)
  (test-flow--call-first-defined
   '(test-flow-detect-runner
     test-flow-runner-detect
     test-flow-determine-runner)))

;;;###autoload
(defun test-flow-cmd-copy-last-report ()
  "Copy path to the last test report to the kill-ring (wrapper)."
  (interactive)
  (test-flow--call-first-defined
   '(test-flow-copy-last-report
     test-flow-report-copy-last
     test-flow-copy-report)))

;;;###autoload
(defun test-flow-cmd-toggle-panel ()
  "Toggle Test Flow panel (wrapper)."
  (interactive)
  (test-flow--call-first-defined
   '(test-flow-view-controls-toggle
     test-flow-toggle-panel
     test-flow-panel-toggle)))

;;;###autoload
(defun test-flow-cmd-toggle-watch ()
  "Toggle automatic test running (watch) (wrapper)."
  (interactive)
  (test-flow--call-first-defined
   '(test-flow-toggle-watch)))

;;;###autoload
(defun test-flow-cmd-show-progress ()
  "Show live progress/output for current panel/session (wrapper)."
  (interactive)
  (test-flow--call-first-defined
   '(test-flow-show-progress)))

;;;###autoload
(defun test-flow-cmd-toggle-headerline ()
  "Toggle Test Flow headerline (wrapper)."
  (interactive)
  (test-flow--call-first-defined
   '(test-flow-headerline-toggle)))

;; -----------------------------------------------------------------------------
;; Transient prefix
;; -----------------------------------------------------------------------------

;; Define a lightweight fallback if transient is missing.
(unless (require 'transient nil 'noerror)
  (defun test-flow-transient ()
    "Fallback: inform that transient is missing."
    (interactive)
    (message "%s" (test-flow-i18n 'transient-missing))))

(when (featurep 'transient)
  (eval-and-compile (require 'transient))
  ;;;###autoload (autoload 'test-flow-transient "test-flow-transient" nil t)
  (transient-define-prefix test-flow-transient ()
    "Transient prefix for global Test Flow actions."
    [:description (lambda () (test-flow-i18n 'title))
                  [:description (lambda () (test-flow-i18n 'sec-run))
                                ("a" test-flow-cmd-run-all         :description (lambda () (test-flow-i18n 'run-all)))
                                ("i" test-flow-cmd-run-file        :description (lambda () (test-flow-i18n 'run-file)))
                                ("t" test-flow-cmd-run-at-point    :description (lambda () (test-flow-i18n 'run-at-point)))
                                ("d" test-flow-cmd-detect-runner   :description (lambda () (test-flow-i18n 'runner-detect)))
                                ("r" test-flow-cmd-rerun           :description (lambda () (test-flow-i18n 'rerun)))]
                  [:description (lambda () (test-flow-i18n 'sec-coverage))
                                ("c" test-flow-cmd-coverage-toggle :description (lambda () (test-flow-i18n 'cov-toggle)))
                                ("s" test-flow-cmd-coverage-show   :description (lambda () (test-flow-i18n 'cov-show)))
                                ("u" test-flow-cmd-coverage-refresh :description (lambda () (test-flow-i18n 'cov-refresh)))]
                  [:description (lambda () (test-flow-i18n 'sec-report))
                                ("o" test-flow-cmd-open-report     :description (lambda () (test-flow-i18n 'report-open)))
                                ("l" test-flow-cmd-open-last-log   :description (lambda () (test-flow-i18n 'log-open)))
                                ("y" test-flow-cmd-copy-last-report :description (lambda () (test-flow-i18n 'report-copy)))]
                  [:description (lambda () (test-flow-i18n 'sec-ui))
                                ("p" test-flow-cmd-toggle-panel    :description (lambda () (test-flow-i18n 'panel-toggle)))
                                ("f" test-flow-cmd-toggle-panel    :description (lambda () (test-flow-i18n 'panel-toggle)))
                                ("w" test-flow-cmd-toggle-watch    :description (lambda () (test-flow-i18n 'watch-toggle)))
                                ("h" test-flow-cmd-toggle-headerline :description (lambda () (test-flow-i18n 'headerline-toggle)))
                                ("$" test-flow-cmd-show-progress   :description (lambda () (test-flow-i18n 'show-progress)))] ]))

;; -----------------------------------------------------------------------------
;; Global minor mode and keybindings
;; -----------------------------------------------------------------------------

(defcustom test-flow-global-transient-key "C-c f"
  "Keybinding (string) to invoke `test-flow-transient'."
  :type 'string
  :group 'test-flow)

(defcustom test-flow-global-transient-enable t
  "Enable `test-flow-global-mode' on load."
  :type 'boolean
  :group 'test-flow)

(defcustom test-flow-global-transient-override-existing t
  "If non-nil, bind keys even if they are already taken in `global-map'."
  :type 'boolean
  :group 'test-flow)

(defvar test-flow-global-mode-map (make-sparse-keymap)
  "Keymap for `test-flow-global-mode'.")

(defvar test-flow--current-transient-key nil)
(defvar test-flow--current-panel-key nil)
(defvar test-flow--current-detect-key nil)
(defvar test-flow--current-copy-report-key nil)

(defun test-flow--kbd (keystr)
  (when (and (stringp keystr) (not (string-empty-p keystr)))
    (condition-case _
        (kbd keystr)
      (error (message "%s" (format (test-flow-i18n 'invalid-key) keystr))
             nil))))

(defun test-flow--conflict-p (key)
  "Check if KEY (vector) is already bound in `global-map'."
  (when key
    (let ((global (lookup-key (current-global-map) key)))
      (and global (not (integerp global))))))

(defun test-flow--bind (sym-current keystr fn)
  "Bind KEYSTR to FN in `test-flow-global-mode-map'.
SYM-CURRENT is a symbol holding currently active key string."
  (let* ((old (symbol-value sym-current))
         (old-k (test-flow--kbd old))
         (new-k (test-flow--kbd keystr)))
    (when old-k
      (define-key test-flow-global-mode-map old-k nil))
    (when (and new-k
               (or test-flow-global-transient-override-existing
                   (not (test-flow--conflict-p new-k))))
      (define-key test-flow-global-mode-map new-k fn)
      (message (test-flow-i18n 'bound-applied)
               keystr fn)
      (set sym-current keystr))
    (when (and new-k
               (not test-flow-global-transient-override-existing)
               (test-flow--conflict-p new-k))
      (let* ((gb (key-binding new-k 'accept-default 'no-remap)))
        (message (test-flow-i18n 'bound-conflict)
                 keystr gb)))))

(defun test-flow--apply-global-bindings ()
  (test-flow--bind 'test-flow--current-transient-key
                   test-flow-global-transient-key
                   #'test-flow-transient))

(defun test-flow--remove-global-bindings ()
  (dolist (k (list test-flow--current-transient-key
                   test-flow--current-panel-key
                   test-flow--current-detect-key
                   test-flow--current-copy-report-key))
    (when-let ((kk (test-flow--kbd k)))
      (define-key test-flow-global-mode-map kk nil)))
  (setq test-flow--current-transient-key nil
        test-flow--current-panel-key nil
        test-flow--current-detect-key nil
        test-flow--current-copy-report-key nil))

;;;###autoload
(define-minor-mode test-flow-global-mode
  "Global minor mode for Test Flow keybindings."
  :global t
  :group 'test-flow
  :init-value nil
  (if test-flow-global-mode
      (test-flow--apply-global-bindings)
    (test-flow--remove-global-bindings)))

;;;###autoload
(defun test-flow-set-global-transient-key (key)
  "Interactively set `test-flow-global-transient-key' to KEY and rebind."
  (interactive
   (list (key-description (read-key-sequence "New key for test-flow-transient: "))))
  (let ((old test-flow-global-transient-key))
    (setq test-flow-global-transient-key key)
    (when test-flow-global-mode
      (test-flow--bind 'test-flow--current-transient-key key #'test-flow-transient))
    (message (test-flow-i18n 'key-updated) old key)))

;; Enable on load if requested.
(when test-flow-global-transient-enable
  (test-flow-global-mode 1))

(provide 'test-flow-transient)

;;; test-flow-transient.el ends here
