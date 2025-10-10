;;; test-flow-view-controls.el --- Header-line controls for test-flow  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Declarative controls (toggles + actions) for test-flow header-line.
;; - Registry with logic and appearance
;; - Order with :gap markers
;; - Renderer that builds clickable header-line segments with icon/text fallback
;; - Header-line face application with buffer-local face-remap cookie
;;
;; No hard require of test-flow to avoid cycles; functions are declared and called when available.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'test-flow-controls-icons nil t)

;; Declarations (provided by modular files; avoid hard requires)
(declare-function test-flow-run "test-flow-runner" ())
(declare-function test-flow-run-failed "test-flow-runner" ())
(declare-function test-flow-toggle-watch "test-flow-watch" ())
(declare-function test-flow-copy-failures "test-flow-panel" ())
(declare-function test-flow-clear "test-flow-panel" ())
(declare-function test-flow-detect-runner "test-flow-runner" ())
(declare-function test-flow-goto-definition-at-point "test-flow-panel" ())
(declare-function test-flow-list-sessions "test-flow-panel" ())
(declare-function test-flow-dashboard "test-flow-panel" ())
(declare-function test-flow--find-panel-session "test-flow-panel" ())
(declare-function test-flow-toggle-logging "test-flow-panel" ())
(declare-function test-flow-status-toggle "test-flow-panel" ())
(declare-function test-flow-status-visible-p "test-flow-panel" (&optional root))
(defvar test-flow-log-enabled nil)
(defvar test-flow-toolbar-style 'auto)

;;;###autoload
(defgroup test-flow-view-controls nil
  "Header-line controls for test-flow."
  :group 'test-flow)

(defface test-flow-headerline
  '((t :inherit default
       :background unspecified))
  "Default header-line face for test-flow panel buffers.
Border color is taken from `test-flow-headerline-border' face."
  :group 'test-flow-view-controls)

(defface test-flow-modeline
  '((t :inherit default))
  "Mode-line face for test-flow panel buffers (inherits from `default`)."
  :group 'test-flow-view-controls)

(defface test-flow-headerline-border
  '((t :inherit shadow))
  "Border face for Test Flow header-line box (its foreground is used)."
  :group 'test-flow-view-controls)

(defvar-local test-flow--headerline-face-cookie nil
  "Face-remap cookie for remapping header-line face in test-flow panel buffer.")

(defvar-local test-flow--modeline-face-cookie nil
  "Face-remap cookie for remapping mode-line face in test-flow panel buffer.")

(defun test-flow-view-controls--panel-buffers ()
  "Return list of test-flow panel buffers."
  (cl-loop for buf in (buffer-list)
           when (with-current-buffer buf (eq major-mode 'test-flow-panel-mode))
           collect buf))

(defun test-flow-view-controls--ensure-headerline-face ()
  "Ensure test-flow panel buffers use `test-flow-headerline' on header-line and `test-flow-modeline' on mode-line."
  (dolist (buf (test-flow-view-controls--panel-buffers))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        ;; Header-line remap
        (when test-flow--headerline-face-cookie
          (ignore-errors
            (face-remap-remove-relative test-flow--headerline-face-cookie))
          (setq test-flow--headerline-face-cookie nil))
        (let* ((def-bg (face-background 'default nil t))
               (box-col (or (face-foreground 'test-flow-headerline-border nil t)
                            (face-foreground 'shadow nil t)
                            (face-foreground 'mode-line nil t)
                            (face-foreground 'default nil t)))
               (spec `(:inherit default
                                :foreground unspecified
                                :background ,def-bg
                                :box (:line-width 1 :color ,box-col))))
          (setq test-flow--headerline-face-cookie
                (face-remap-add-relative 'header-line spec)))
        ;; Mode-line remap
        (when test-flow--modeline-face-cookie
          (ignore-errors
            (face-remap-remove-relative test-flow--modeline-face-cookie))
          (setq test-flow--modeline-face-cookie nil))
        (setq test-flow--modeline-face-cookie
              (face-remap-add-relative 'mode-line 'test-flow-modeline))
        (force-mode-line-update t)))))

;; Apply face now if buffers already exist.
(test-flow-view-controls--ensure-headerline-face)

(defcustom test-flow-headerline-controls-order
  '(stats :gap run run-failed :gap watch :gap copy clear :gap detect goto :gap sessions dashboard :gap logging)
  "Order of controls in header-line. Use :gap for spacing."
  :type '(repeat (choice symbol (const :gap)))
  :group 'test-flow-view-controls)

;; Helpers to read session/watch state safely
(defun test-flow-view-controls--log (fmt &rest args)
  "Internal: log from view-controls when test-flow logging is enabled."
  (when (and (boundp 'test-flow-log-enabled) test-flow-log-enabled)
    (apply #'message (concat "[test-flow ctl] " fmt) args)))

(defun test-flow-view-controls--watch-state ()
  "Return 'on or 'off for watch toggle in the current panel buffer, with diagnostics."
  (let ((on nil) (root nil))
    (ignore-errors
      (when (fboundp 'test-flow--find-panel-session)
        (let ((s (test-flow--find-panel-session)))
          (when (and s (fboundp 'test-flow--session-root))
            (setq root (test-flow--session-root s)))
          ;; Prefer safe getter when available
          (cond
           ((and s (fboundp 'test-flow--get-watch-enabled))
            (setq on (test-flow--get-watch-enabled s)))
           ((and s (fboundp 'test-flow--session-watch-enabled))
            (setq on (test-flow--session-watch-enabled s)))))))
    (test-flow-view-controls--log "watch-state: panel=%s root=%s on=%s"
                                  (buffer-name) (or root "?") (if on "t" "nil"))
    (if on 'on 'off)))

(defun test-flow-view-controls--status-split-state ()
  "Return 'on or 'off for Status split toggle in the current panel buffer."
  (let ((root nil))
    (ignore-errors
      (when (fboundp 'test-flow--find-panel-session)
        (let ((s (test-flow--find-panel-session)))
          (when (and s (fboundp 'test-flow--session-root))
            (setq root (test-flow--session-root s))))))
    (let ((vis (and (fboundp 'test-flow-status-visible-p)
                    (test-flow-status-visible-p root))))
      (test-flow-view-controls--log "status-split: panel=%s root=%s visible=%s"
                                    (buffer-name) (or root "?") (if vis "t" "nil"))
      (if vis 'on 'off))))

(defcustom test-flow-controls-registry
  `(
    (stats
     :type toggle
     :icon-key stats
     :command test-flow-status-toggle
     :help "Toggle status split (q)"
     :enabled-p ,(lambda () t)
     :visible-p ,(lambda () t)
     :state-fn ,#'test-flow-view-controls--status-split-state
     :label-fn ,(lambda (style state)
                  (pcase style
                    ((or 'icons 'auto) " [Σ]")
                    (_ (format " [Status: %s]" (if (eq state 'on) "On" "Off"))))))
    (run
     :type action
     :icon-key run
     :command test-flow-run
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
     :command test-flow-run-failed
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
     :command test-flow-toggle-watch
     :help "Toggle watch (w)"
     :enabled-p ,(lambda () t)
     :visible-p ,(lambda () t)
     :state-fn ,#'test-flow-view-controls--watch-state
     :label-fn ,(lambda (style state)
                  (pcase style
                    ((or 'icons 'auto) " [W]")
                    (_ (format " [Watch: %s]" (if (eq state 'on) "On" "Off"))))))
    (copy
     :type action
     :icon-key copy
     :command test-flow-copy-failures
     :help "Copy failures (c)"
     :enabled-p ,(lambda () t)
     :visible-p ,(lambda () t)
     :label-fn ,(lambda (style _) (if (eq style 'text) " [Copy]" " [📋]")))
    (clear
     :type action
     :icon-key clear
     :command test-flow-clear
     :help "Clear panel (x)"
     :enabled-p ,(lambda () t)
     :visible-p ,(lambda () t)
     :label-fn ,(lambda (style _) (if (eq style 'text) " [Clear]" " [C]")))
    (detect
     :type action
     :icon-key detect
     :command test-flow-detect-runner
     :help "Detect runner (d)"
     :enabled-p ,(lambda () t)
     :visible-p ,(lambda () t)
     :label-fn ,(lambda (style _) (if (eq style 'text) " [Detect]" " [🔎]")))
    (goto
     :type action
     :icon-key goto
     :command test-flow-goto-definition-at-point
     :help "Goto test definition (o)"
     :enabled-p ,(lambda () t)
     :visible-p ,(lambda () t)
     :label-fn ,(lambda (style _) (if (eq style 'text) " [Goto]" " [↗]")))
    (sessions
     :type action
     :icon-key sessions
     :command test-flow-list-sessions
     :help "List sessions"
     :enabled-p ,(lambda () (fboundp 'test-flow-list-sessions))
     :visible-p ,(lambda () t)
     :label-fn ,(lambda (style _) (if (eq style 'text) " [Sessions]" " [S]")))
    (dashboard
     :type action
     :icon-key dashboard
     :command test-flow-dashboard
     :help "Dashboard"
     :enabled-p ,(lambda () (fboundp 'test-flow-dashboard))
     :visible-p ,(lambda () t)
     :label-fn ,(lambda (style _) (if (eq style 'text) " [Dashboard]" " [D]")))
    (logging
     :type toggle
     :icon-key logging
     :command test-flow-toggle-logging
     :help "Toggle logging"
     :enabled-p ,(lambda () t)
     :visible-p ,(lambda () t)
     :state-fn ,(lambda () (if (and (boundp 'test-flow-log-enabled) test-flow-log-enabled) 'on 'off))
     :label-fn ,(lambda (style state)
                  (pcase style
                    ((or 'icons 'auto) " [L]")
                    (_ (format " [Log: %s]" (if (eq state 'on) "On" "Off"))))))
    )
  "Registry of test-flow header-line controls."
  :type '(alist :key-type symbol :value-type plist)
  :group 'test-flow-view-controls)

(defun test-flow-view-controls--plist-fn (val)
  "If VAL is a function, call it with no args, else return VAL."
  (if (functionp val) (funcall val) val))

(defun test-flow-view-controls--render (key)
  "Render a single control segment for KEY, or nil if hidden."
  (let* ((desc (alist-get key test-flow-controls-registry))
         (type (plist-get desc :type))
         (cmd  (plist-get desc :command))
         (help (plist-get desc :help))
         (enabled-p (let ((fn (or (plist-get desc :enabled-p) (lambda () t))))
                      (ignore-errors (funcall fn))))
         (visible-p (let ((fn (or (plist-get desc :visible-p) (lambda () t))))
                      (ignore-errors (funcall fn))))
         (style test-flow-toolbar-style))
    (when (and desc visible-p)
      (let* ((gicons (and (fboundp 'test-flow-controls-icons-available-p)
                          (test-flow-controls-icons-available-p)))
             (style* (cond
                      ((eq style 'text) 'text)
                      ((and (memq style '(auto icons)) gicons) 'icons)
                      (t 'text)))
             (state (when (eq type 'toggle)
                      (let ((fn (plist-get desc :state-fn)))
                        (when (functionp fn) (ignore-errors (funcall fn))))))
             (spinner (let ((fn (plist-get desc :spinner-fn)))
                        (when (functionp fn) (ignore-errors (funcall fn)))))
             (icon-key (plist-get desc :icon-key))
             (ico (and gicons (not spinner)
                       (test-flow-controls-icon icon-key state)))
             (label (cond
                     (spinner (concat " " spinner))
                     (ico     (concat " " ico))
                     (t (let ((lf (plist-get desc :label-fn)))
                          (when (functionp lf) (funcall lf style* state))))))
             (s (copy-sequence (or label "")))
             (help-str (test-flow-view-controls--plist-fn help)))
        (when (> (length s) 0)
          (let ((len (length s)))
            ;; Tooltip на весь сегмент; действие — тоже на весь сегмент (глобальная карта его найдёт).
            (add-text-properties 0 len (list 'help-echo help-str) s)
            (when (and cmd enabled-p (symbolp cmd))
              (add-text-properties 0 len (list 'test-flow-action cmd) s))
            ;; Разделяем сегменты: не подсвечиваем ведущий пробел, чтобы mouse-face не сливался с соседями.
            (when (> len 0)
              ;; Левый пробел — стрелка, без подсветки
              (add-text-properties 0 (min 1 len) (list 'pointer 'arrow) s)
              (if (and cmd enabled-p)
                  ;; Кликабельная часть — с 1 по конец: рука + подсветка
                  (add-text-properties 1 len (list 'pointer 'hand 'mouse-face 'highlight) s)
                ;; Неактивно/нет команды — рука не нужна, подсветки нет
                (add-text-properties 1 len (list 'pointer 'arrow) s))))
          ;; Текстовый фейс при отсутствии иконок
          (when-let* ((ff (plist-get desc :face-fn))
                      (face (and (functionp ff) (funcall ff style* state))))
            (unless gicons
              (add-text-properties 0 (length s)
                                   (list 'face (or (and (symbolp face) face)
                                                   (and (listp face) face)))
                                   s)))
          ;; Затенение, если выключено
          (unless enabled-p
            (add-text-properties 0 (length s) (list 'face 'shadow) s))
          s)))))

(defun test-flow-view-controls--gap ()
  "Return a single-space gap segment (без действия)."
  (let ((s " "))
    (add-text-properties 0 (length s)
                         (list 'mouse-face nil
                               'help-echo nil
                               'pointer 'arrow)
                         s)
    s))

(defun test-flow-view-controls-segments (&optional _where)
  "Return ordered control segments for the header-line as a list of strings."
  (let* ((order test-flow-headerline-controls-order)
         (res '()))
    (dolist (k order)
      (if (eq k :gap)
          (push (test-flow-view-controls--gap) res)
        (when-let* ((seg (test-flow-view-controls--render k)))
          (when (stringp seg)
            (push seg res)))))
    (nreverse res)))

;; Auto-refresh UI when registry/order change.
(when (fboundp 'add-variable-watcher)
  (dolist (sym '(test-flow-headerline-controls-order
                 test-flow-controls-registry))
    (add-variable-watcher
     sym
     (lambda (&rest _)
       (ignore-errors
         (dolist (buf (test-flow-view-controls--panel-buffers))
           (when (buffer-live-p buf)
             (with-current-buffer buf
               (setq-local test-flow-headerline--cache-key nil)
               (setq-local test-flow-headerline--cache-str nil)))))
       (test-flow-view-controls--ensure-headerline-face)
       (force-mode-line-update t)))))

(provide 'test-flow-view-controls)
;;; test-flow-view-controls.el ends here
