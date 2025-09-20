;;; ert-flow-view-controls.el --- Header-line controls for ert-flow  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Declarative controls (toggles + actions) for ert-flow header-line.
;; - Registry with logic and appearance
;; - Order with :gap markers
;; - Renderer that builds clickable header-line segments with icon/text fallback
;; - Header-line face application with buffer-local face-remap cookie
;;
;; No hard require of ert-flow to avoid cycles; functions are declared and called when available.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'ert-flow-controls-icons)

;; Declarations (provided by ert-flow.el)
(declare-function ert-flow-run "ert-flow" ())
(declare-function ert-flow-run-failed "ert-flow" ())
(declare-function ert-flow-toggle-watch "ert-flow" ())
(declare-function ert-flow-copy-failures "ert-flow" ())
(declare-function ert-flow-clear "ert-flow" ())
(declare-function ert-flow-detect-runner "ert-flow" ())
(declare-function ert-flow-goto-definition-at-point "ert-flow" ())
(declare-function ert-flow-list-sessions "ert-flow" ())
(declare-function ert-flow-dashboard "ert-flow" ())
(declare-function ert-flow--find-panel-session "ert-flow" ())
(declare-function ert-flow-toggle-logging "ert-flow" ())
(defvar ert-flow-log-enabled nil)
(defvar ert-flow-toolbar-style 'auto)

(defgroup ert-flow-view-controls nil
  "Header-line controls for ert-flow."
  :group 'ert-flow)

(defface ert-flow-headerline
  '((t :background "#2f2f2f" :foreground "gray90"))
  "Default header-line face for ert-flow panel buffers."
  :group 'ert-flow-view-controls)

(defvar-local ert-flow--headerline-face-cookie nil
  "Face-remap cookie for remapping header-line face in ert-flow panel buffer.")

(defun ert-flow-view-controls--panel-buffers ()
  "Return list of ert-flow panel buffers."
  (cl-loop for buf in (buffer-list)
           when (with-current-buffer buf (eq major-mode 'ert-flow-panel-mode))
           collect buf))

(defun ert-flow-view-controls--ensure-headerline-face ()
  "Ensure ert-flow panel buffers use `ert-flow-headerline' for header-line."
  (dolist (buf (ert-flow-view-controls--panel-buffers))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when ert-flow--headerline-face-cookie
          (ignore-errors
            (face-remap-remove-relative ert-flow--headerline-face-cookie))
          (setq ert-flow--headerline-face-cookie nil))
        (setq ert-flow--headerline-face-cookie
              (face-remap-add-relative 'header-line 'ert-flow-headerline))
        (force-mode-line-update t)))))

;; Apply face now if buffers already exist.
(ert-flow-view-controls--ensure-headerline-face)

(defcustom ert-flow-headerline-controls-order
  '(run run-failed :gap watch :gap copy clear :gap detect goto :gap sessions dashboard :gap logging)
  "Order of controls in header-line. Use :gap for spacing."
  :type '(repeat (choice symbol (const :gap)))
  :group 'ert-flow-view-controls)

;; Helpers to read session/watch state safely
(defun ert-flow-view-controls--watch-state ()
  "Return 'on or 'off for watch toggle in the current panel buffer."
  (let ((on nil))
    (ignore-errors
      (when (fboundp 'ert-flow--find-panel-session)
        (let ((s (ert-flow--find-panel-session)))
          ;; Accessor defined by cl-defstruct in ert-flow.el
          (when (and s (fboundp 'ert-flow--session-watch-enabled))
            (setq on (ert-flow--session-watch-enabled s))))))
    (if on 'on 'off)))

(defcustom ert-flow-controls-registry
  `(
    (run
     :type action
     :icon-key run
     :command ert-flow-run
     :help "Run all tests (r)"
     :enabled-p ,(lambda () t)
     :visible-p ,(lambda () t)
     :label-fn ,(lambda (style _)
                  (pcase style
                    ((or 'icons 'auto) " [▶]")
                    (_ " [Run]"))))
    (run-failed
     :type action
     :icon-key run-failed
     :command ert-flow-run-failed
     :help "Run failed tests if available (f)"
     :enabled-p ,(lambda () t)
     :visible-p ,(lambda () t)
     :label-fn ,(lambda (style _)
                  (pcase style
                    ((or 'icons 'auto) " [↻]")
                    (_ " [Run failed]"))))
    (watch
     :type toggle
     :icon-key watch
     :command ert-flow-toggle-watch
     :help "Toggle watch (w)"
     :enabled-p ,(lambda () t)
     :visible-p ,(lambda () t)
     :state-fn ,#'ert-flow-view-controls--watch-state
     :label-fn ,(lambda (style state)
                  (pcase style
                    ((or 'icons 'auto) " [W]")
                    (_ (format " [Watch: %s]" (if (eq state 'on) "On" "Off"))))))
    (copy
     :type action
     :icon-key copy
     :command ert-flow-copy-failures
     :help "Copy failures (c)"
     :enabled-p ,(lambda () t)
     :visible-p ,(lambda () t)
     :label-fn ,(lambda (style _) (if (eq style 'text) " [Copy]" " [📋]")))
    (clear
     :type action
     :icon-key clear
     :command ert-flow-clear
     :help "Clear panel (x)"
     :enabled-p ,(lambda () t)
     :visible-p ,(lambda () t)
     :label-fn ,(lambda (style _) (if (eq style 'text) " [Clear]" " [C]")))
    (detect
     :type action
     :icon-key detect
     :command ert-flow-detect-runner
     :help "Detect runner (d)"
     :enabled-p ,(lambda () t)
     :visible-p ,(lambda () t)
     :label-fn ,(lambda (style _) (if (eq style 'text) " [Detect]" " [🔎]")))
    (goto
     :type action
     :icon-key goto
     :command ert-flow-goto-definition-at-point
     :help "Goto test definition (o)"
     :enabled-p ,(lambda () t)
     :visible-p ,(lambda () t)
     :label-fn ,(lambda (style _) (if (eq style 'text) " [Goto]" " [↗]")))
    (sessions
     :type action
     :icon-key sessions
     :command ert-flow-list-sessions
     :help "List sessions"
     :enabled-p ,(lambda () (fboundp 'ert-flow-list-sessions))
     :visible-p ,(lambda () t)
     :label-fn ,(lambda (style _) (if (eq style 'text) " [Sessions]" " [S]")))
    (dashboard
     :type action
     :icon-key dashboard
     :command ert-flow-dashboard
     :help "Dashboard"
     :enabled-p ,(lambda () (fboundp 'ert-flow-dashboard))
     :visible-p ,(lambda () t)
     :label-fn ,(lambda (style _) (if (eq style 'text) " [Dashboard]" " [D]")))
    (logging
     :type toggle
     :icon-key logging
     :command ert-flow-toggle-logging
     :help "Toggle logging"
     :enabled-p ,(lambda () t)
     :visible-p ,(lambda () t)
     :state-fn ,(lambda () (if (and (boundp 'ert-flow-log-enabled) ert-flow-log-enabled) 'on 'off))
     :label-fn ,(lambda (style state)
                  (pcase style
                    ((or 'icons 'auto) " [L]")
                    (_ (format " [Log: %s]" (if (eq state 'on) "On" "Off"))))))
    )
  "Registry of ert-flow header-line controls."
  :type '(alist :key-type symbol :value-type plist)
  :group 'ert-flow-view-controls)

(defun ert-flow-view-controls--plist-fn (val)
  "If VAL is a function, call it with no args, else return VAL."
  (if (functionp val) (funcall val) val))

(defun ert-flow-view-controls--render (key)
  "Render a single control segment for KEY, or nil if hidden/disabled."
  (let* ((desc (alist-get key ert-flow-controls-registry))
         (type (plist-get desc :type))
         (cmd  (plist-get desc :command))
         (help (plist-get desc :help))
         (enabled-p (let ((fn (or (plist-get desc :enabled-p) (lambda () t))))
                      (ignore-errors (funcall fn))))
         (visible-p (let ((fn (or (plist-get desc :visible-p) (lambda () t))))
                      (ignore-errors (funcall fn))))
         (style ert-flow-toolbar-style))
    (when (and desc visible-p)
      (let* ((gicons (and (fboundp 'ert-flow-controls-icons-available-p)
                          (ert-flow-controls-icons-available-p)))
             (state (when (eq type 'toggle)
                      (let ((fn (plist-get desc :state-fn)))
                        (when (functionp fn) (ignore-errors (funcall fn))))))
             (spinner (let ((fn (plist-get desc :spinner-fn)))
                        (when (functionp fn) (ignore-errors (funcall fn)))))
             (icon-key (plist-get desc :icon-key))
             (ico (and gicons (not spinner)
                       (ert-flow-controls-icon icon-key state)))
             (label (cond
                     (spinner (concat " " spinner))
                     (ico     (concat " " ico))
                     (t (let ((lf (plist-get desc :label-fn)))
                          (when (functionp lf) (funcall lf style state))))))
             (s (copy-sequence (or label "")))
             (beg (if (and (> (length s) 0) (eq (aref s 0) ?\s)) 1 0))
             (km (when (and cmd enabled-p)
                   (let ((m (make-sparse-keymap)))
                     (define-key m [mouse-1] cmd)
                     (define-key m [header-line mouse-1] cmd)
                     m)))
             (help-str (ert-flow-view-controls--plist-fn help)))
        (when (> (length s) 0)
          (let ((props (list 'mouse-face 'highlight
                             'help-echo help-str
                             'ert-flow-key key)))
            (when km
              (setq props (append props (list 'keymap km 'local-map km))))
            (when (eq type 'toggle)
              (setq props (append props (list 'ert-flow-toggle key))))
            (when (eq type 'action)
              (setq props (append props (list 'ert-flow-action key))))
            (add-text-properties beg (length s) props s))
          (unless enabled-p
            (add-text-properties beg (length s) (list 'face 'shadow) s))
          s)))))

(defun ert-flow-view-controls-segments (&optional _where)
  "Return ordered control segments for the header-line as a list of strings."
  (let* ((order ert-flow-headerline-controls-order)
         (res '()))
    (dolist (k order)
      (if (eq k :gap)
          (push " " res)
        (when-let* ((seg (ert-flow-view-controls--render k)))
          (when (stringp seg)
            (push seg res)))))
    (nreverse res)))

;; Auto-refresh UI when registry/order change.
(when (fboundp 'add-variable-watcher)
  (dolist (sym '(ert-flow-headerline-controls-order
                 ert-flow-controls-registry))
    (add-variable-watcher
     sym
     (lambda (&rest _)
       (ignore-errors
         (dolist (buf (ert-flow-view-controls--panel-buffers))
           (when (buffer-live-p buf)
             (with-current-buffer buf
               (setq-local ert-flow-headerline--cache-key nil)
               (setq-local ert-flow-headerline--cache-str nil)))))
       (ert-flow-view-controls--ensure-headerline-face)
       (force-mode-line-update t)))))

(provide 'ert-flow-view-controls)
;;; ert-flow-view-controls.el ends here
