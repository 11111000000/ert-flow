;;; test-flow-controls-icons.el --- Graphic icons for test-flow controls  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Icons provider for test-flow header-line controls.
;; - Decides availability (GUI + all-the-icons)
;; - Renders icons with uniform size and optional raise
;; - Caches rendered strings
;; - Refreshes UI when icons/settings/theme change

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(declare-function test-flow-headerline-refresh "test-flow-headerline" ())

;;;###autoload
(defgroup test-flow-controls-icons nil
  "Graphic icons for test-flow header-line controls."
  :group 'test-flow
  :prefix "test-flow-controls-")

(defcustom test-flow-controls-use-graphic-icons t
  "Prefer graphic icons (all-the-icons) for controls when available."
  :type 'boolean
  :group 'test-flow-controls-icons)

(defcustom test-flow-controls-icon-height 0.9
  "Uniform height for control icons."
  :type 'number
  :group 'test-flow-controls-icons)

(defcustom test-flow-controls-icon-raise 0.11
  "Vertical raise for control icons (applied via `display' property)."
  :type 'number
  :group 'test-flow-controls-icons)

(defface test-flow-controls-icon-on
  '((t :inherit success))
  "Face for ON-state control icons."
  :group 'test-flow-controls-icons)

(defface test-flow-controls-icon-off
  '((t :inherit shadow))
  "Face for OFF-state control icons."
  :group 'test-flow-controls-icons)

;; Default icon map for controls (single or stateful (on/off)).
(defcustom test-flow-controls-icon-map
  '((stats       . (material . "bar_chart"))
    (run         . (material . "play_arrow"))
    (run-failed  . (material . "replay"))
    (watch       . ((on  . (material . "visibility"))
                    (off . (material . "visibility_off"))))
    (copy        . (faicon   . "clipboard"))
    (clear       . (material . "delete"))
    (detect      . (material . "search"))
    (goto        . (octicon  . "arrow-right"))
    (sessions    . (material . "view_list"))
    (dashboard   . (material . "dashboard"))
    (logging     . ((on  . (faicon . "bug"))
                    (off . (faicon . "bug")))))
  "Mapping of control keys to all-the-icons specs.
Either a cons (PROVIDER . NAME), or an alist of ((on . (PROVIDER . NAME)) (off . ...))."
  :type '(alist :key-type symbol
                :value-type (choice
                             (cons (symbol :tag "Provider") (string :tag "Name"))
                             (alist :key-type (choice (const on) (const off))
                                    :value-type (cons (symbol :tag "Provider")
                                                      (string :tag "Name")))))
  :group 'test-flow-controls-icons)

(defcustom test-flow-controls-icon-face-map
  '((stats      . (:foreground "LightSkyBlue3"))
    (run        . (:foreground "SpringGreen3"))
    (run-failed . (:foreground "DarkOrange2"))
    (copy       . (:foreground "SteelBlue3"))
    (clear      . (:foreground "tomato"))
    (detect     . (:foreground "MediumPurple3"))
    (goto       . (:foreground "Gold3"))
    (sessions   . (:foreground "gray70"))
    (dashboard  . (:foreground "gray70")))
  "Optional face overrides for non-toggle icons (symbol or plist)."
  :type '(alist :key-type symbol
                :value-type (choice face (plist :key-type symbol :value-type sexp)))
  :group 'test-flow-controls-icons)

(defcustom test-flow-controls-toggle-on-face
  '(:foreground "gray85")
  "Face attributes/symbol for toggle icons when ON."
  :type '(choice face (plist :key-type symbol :value-type sexp))
  :group 'test-flow-controls-icons)

(defcustom test-flow-controls-toggle-off-face
  '(:foreground "gray60")
  "Face attributes/symbol for toggle icons when OFF."
  :type '(choice face (plist :key-type symbol :value-type sexp))
  :group 'test-flow-controls-icons)

(defvar test-flow-controls-icons--cache (make-hash-table :test 'equal)
  "Cache for rendered icons. Key: (KEY STATE HEIGHT RAISE FACE).")

(defun test-flow-controls-icons--provider-fn (provider)
  "Return all-the-icons function for PROVIDER symbol, or nil."
  (let* ((name (format "all-the-icons-%s" provider))
         (fn (intern-soft name)))
    (when (and fn (fboundp fn)) fn)))

(defun test-flow-controls-icons--spec-for (key state)
  "Return icon spec cons (PROVIDER . NAME) for KEY and optional STATE."
  (let ((spec (alist-get key test-flow-controls-icon-map)))
    (cond
     ((and (consp spec) (symbolp (car spec)) (stringp (cdr spec))) spec)
     ((and (listp spec) (memq state '(on off))) (alist-get state spec))
     (t nil))))

(defun test-flow-controls-icons-available-p ()
  "Return non-nil if icons can be used now (GUI + all-the-icons)."
  (and test-flow-controls-use-graphic-icons
       (display-graphic-p)
       (featurep 'all-the-icons)))

(defun test-flow-controls-icon (key &optional state)
  "Return propertized icon string for KEY and optional STATE, or nil."
  (when (test-flow-controls-icons-available-p)
    (let* ((spec (test-flow-controls-icons--spec-for key state))
           (provider (car-safe spec))
           (name (cdr-safe spec))
           (base-face (when (memq state '(on off))
                        (if (eq state 'on)
                            'test-flow-controls-icon-on
                          'test-flow-controls-icon-off)))
           (override (alist-get key test-flow-controls-icon-face-map))
           (final-face
            (cond
             ((eq key 'watch)
              (if (eq state 'on)
                  test-flow-controls-toggle-on-face
                test-flow-controls-toggle-off-face))
             ((eq key 'logging)
              (if (eq state 'on)
                  test-flow-controls-toggle-on-face
                test-flow-controls-toggle-off-face))
             (override override)
             (t base-face)))
           (raise test-flow-controls-icon-raise)
           (cache-key (list key state test-flow-controls-icon-height raise final-face)))
      (or (gethash cache-key test-flow-controls-icons--cache)
          (when (and provider name)
            (let* ((fn (test-flow-controls-icons--provider-fn provider))
                   (icon
                    (when fn
                      (ignore-errors
                        (funcall fn name
                                 :face (or (and (symbolp final-face) final-face)
                                           (and (listp final-face) final-face))
                                 :height test-flow-controls-icon-height
                                 :v-adjust 0.0)))))
              (when (and (stringp icon) (not (string-empty-p icon)))
                (let ((s (propertize icon 'display (list 'raise raise))))
                  (puthash cache-key s test-flow-controls-icons--cache)
                  s))))))))

(defun test-flow-controls-icons-clear-cache ()
  "Clear cached rendered icons."
  (clrhash test-flow-controls-icons--cache))

;; obsolete: `test-flow-headerline-refresh' is provided by test-flow-headerline.el
;; This module calls it from `test-flow-controls-icons--refresh-ui'.

(defun test-flow-controls-icons--refresh-ui ()
  "Refresh UI after icon settings change/load/theme."
  (ignore-errors (test-flow-controls-icons-clear-cache))
  (when (fboundp 'test-flow-headerline-refresh)
    (test-flow-headerline-refresh)))

;; Refresh when all-the-icons loads
(with-eval-after-load 'all-the-icons
  (test-flow-controls-icons--refresh-ui))

;; Refresh on variable changes
(when (fboundp 'add-variable-watcher)
  (dolist (sym '(test-flow-controls-use-graphic-icons
                 test-flow-controls-icon-map
                 test-flow-controls-icon-face-map
                 test-flow-controls-icon-height
                 test-flow-controls-icon-raise))
    (add-variable-watcher
     sym
     (lambda (&rest _) (test-flow-controls-icons--refresh-ui)))))

;; Refresh after theme changes
(when (boundp 'after-enable-theme-functions)
  (add-hook 'after-enable-theme-functions
            (lambda (&rest _) (test-flow-controls-icons--refresh-ui))))

(provide 'test-flow-controls-icons)
;;; test-flow-controls-icons.el ends here
