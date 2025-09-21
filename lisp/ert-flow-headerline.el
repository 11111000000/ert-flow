;;; ert-flow-headerline.el --- Header-line controls for ert-flow  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Builds header-line content for ert-flow panel buffers.
;; - Uses ert-flow-view-controls to generate interactive segments
;; - Light caching keyed by UI-relevant inputs
;; - Feature flag to enable/disable header-line
;; - Applies/removes header-line for panel buffers

;;; Code:

(require 'cl-lib)
(require 'subr-x)

;; Soft deps (optional to avoid hard require cycles)
(require 'ert-flow-view-controls nil t)
(require 'ert-flow-controls-icons nil t)

(defgroup ert-flow-headerline nil
  "Header-line controls for ert-flow."
  :group 'ert-flow)

(defcustom ert-flow-view-headerline-enable t
  "When non-nil, show controls in the header-line of ert-flow panel buffers."
  :type 'boolean :group 'ert-flow-headerline)

;; Cache variables are buffer-local to each panel
(defvar-local ert-flow-headerline--cache-key nil)
(defvar-local ert-flow-headerline--cache-str nil)

(defvar ert-flow--active-run-count 0)
(defvar ert-flow--run-queue nil)

;; Глобальная карта для header-line: гасит нажатия и тянучки, а на отпускание
;; выполняет команду, помеченную свойством `ert-flow-action' под курсором.
(defun ert-flow-headerline--consume (e)
  "Поглотить событие мыши в header-line, не передавая его в буфер."
  (interactive "e")
  nil)

(defun ert-flow-headerline--on-mouse (e)
  "Выполнить команду, помеченную на сегменте под курсором (ert-flow-action)."
  (interactive "e")
  (let* ((pos (event-end e))
         (sp  (posn-string pos)))
    (when (consp sp)
      (let* ((str (car sp))
             (idx (cdr sp))
             (cmd (and (stringp str) (integerp idx)
                       (get-text-property idx 'ert-flow-action str))))
        (when (and (symbolp cmd) (commandp cmd))
          ;; Выполним после отпускания, чтобы гарантированно не задеть буфер.
          (run-at-time 0 nil (lambda () (call-interactively cmd)))))))
  nil)

(defvar ert-flow-headerline--global-map
  (let ((m (make-sparse-keymap)))
    ;; Поглотить нажатие и перетаскивание
    (define-key m [header-line down-mouse-1] #'ert-flow-headerline--consume)
    (define-key m [header-line drag-mouse-1] #'ert-flow-headerline--consume)
    ;; Выполнить на отпускание
    (define-key m [header-line mouse-1] #'ert-flow-headerline--on-mouse)
    ;; Подстраховка на случай «обычных» событий без префикса header-line
    (define-key m [down-mouse-1] #'ert-flow-headerline--consume)
    (define-key m [drag-mouse-1] #'ert-flow-headerline--consume)
    (define-key m [mouse-1] #'ert-flow-headerline--on-mouse)
    m)
  "Глобальная keymap для всей строки header-line.")

(defun ert-flow-headerline--fallback-segments ()
  "Return a minimal textual controls list for header-line as a fallback."
  (let* ((watch-on (ignore-errors
                     (when (fboundp 'ert-flow--find-panel-session)
                       (let ((s (ert-flow--find-panel-session)))
                         (cond
                          ((and s (fboundp 'ert-flow--get-watch-enabled))
                           (ert-flow--get-watch-enabled s))
                          ((and s (fboundp 'ert-flow--session-watch-enabled))
                           (ert-flow--session-watch-enabled s))
                          (t nil))))))
         (gap (lambda ()
                (let ((s " "))
                  (add-text-properties 0 (length s)
                                       (list 'mouse-face nil
                                             'help-echo nil
                                             'pointer 'arrow)
                                       s)
                  s)))
         (mk (lambda (label cmd help)
               (let* ((s (concat " " label))
                      (len (length s)))
                 ;; Tooltip на весь сегмент; действие — на весь сегмент.
                 (add-text-properties 0 len (list 'help-echo help) s)
                 (when (symbolp cmd)
                   (add-text-properties 0 len (list 'ert-flow-action cmd) s))
                 ;; Не подсвечиваем ведущий пробел, чтобы не сливалось с соседями.
                 (when (> len 0)
                   (add-text-properties 0 (min 1 len) (list 'pointer 'arrow) s)
                   (if (symbolp cmd)
                       (add-text-properties 1 len (list 'pointer 'hand 'mouse-face 'highlight) s)
                     (add-text-properties 1 len (list 'pointer 'arrow) s)))
                 s))))
    (list
     (funcall mk "[Run]" #'ert-flow-run "Run all tests (r)")
     (funcall gap)
     (funcall mk "[↻]" #'ert-flow-run-failed "Run failed tests (f)")
     (funcall gap)
     (funcall mk (format "[Watch:%s]" (if watch-on "On" "Off"))
              #'ert-flow-toggle-watch "Toggle watch (w)")
     (funcall gap)
     (funcall mk "[Copy]" #'ert-flow-copy-failures "Copy failures (c)")
     (funcall gap)
     (funcall mk "[Clear]" #'ert-flow-clear "Clear panel (x)")
     (funcall gap)
     (funcall mk "[Detect]" #'ert-flow-detect-runner "Detect runner (d)")
     (funcall gap)
     (funcall mk "[Goto]" #'ert-flow-goto-definition-at-point "Goto test definition (o)"))))

(defun ert-flow-headerline-format ()
  "Return header-line content for ert-flow panel buffers with caching."
  (when (eq major-mode 'ert-flow-panel-mode)
    (let* ((style (and (boundp 'ert-flow-toolbar-style)
                       ert-flow-toolbar-style))
           (icons-on (and (fboundp 'ert-flow-controls-icons-available-p)
                          (ert-flow-controls-icons-available-p)))
           (watch-on (ignore-errors
                       (when (fboundp 'ert-flow--find-panel-session)
                         (let ((s (ert-flow--find-panel-session)))
                           (cond
                            ((and s (fboundp 'ert-flow--get-watch-enabled))
                             (ert-flow--get-watch-enabled s))
                            ((and s (fboundp 'ert-flow--session-watch-enabled))
                             (ert-flow--session-watch-enabled s))
                            (t nil))))))
           (log-on (and (boundp 'ert-flow-log-enabled) ert-flow-log-enabled))
           (active (and (boundp 'ert-flow--active-run-count) ert-flow--active-run-count))
           (queued (and (boundp 'ert-flow--run-queue) (length ert-flow--run-queue)))
           (key (list style icons-on (and watch-on t) (and log-on t) active queued)))
      (let* ((s
              (if (equal key ert-flow-headerline--cache-key)
                  ert-flow-headerline--cache-str
                (let* ((controls (or
                                  (ignore-errors
                                    (when (fboundp 'ert-flow-view-controls-segments)
                                      ;; Лёгкое смещение для иконок в header-line
                                      (let ((ert-flow-controls-icon-raise -0.08))
                                        (ert-flow-view-controls-segments))))
                                  (ert-flow-headerline--fallback-segments)))
                       (out (mapconcat #'identity controls "")))
                  (setq ert-flow-headerline--cache-key key
                        ert-flow-headerline--cache-str out)
                  out))))
        ;; Гарантируем наличие глобальной карты на всей строке (даже если в кэше старая строка).
        (when (stringp s)
          (add-text-properties 0 (length s)
                               (list 'keymap ert-flow-headerline--global-map
                                     'local-map ert-flow-headerline--global-map)
                               s))
        s))))

(defun ert-flow-headerline--apply (buffer)
  "Apply or remove header-line in BUFFER according to feature flag."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (eq major-mode 'ert-flow-panel-mode)
        (if ert-flow-view-headerline-enable
            (setq header-line-format '((:eval (ert-flow-headerline-format))))
          (when (equal header-line-format '((:eval (ert-flow-headerline-format))))
            (setq header-line-format nil)))
        (ignore-errors
          (when (fboundp 'ert-flow-view-controls--ensure-headerline-face)
            (ert-flow-view-controls--ensure-headerline-face)))
        (force-mode-line-update t)))))

;; Watcher: toggling feature flag should apply/remove header-line everywhere.
(when (fboundp 'add-variable-watcher)
  (add-variable-watcher
   'ert-flow-view-headerline-enable
   (lambda (&rest _)
     (dolist (buf (buffer-list))
       (when (buffer-live-p buf)
         (with-current-buffer buf
           (when (eq major-mode 'ert-flow-panel-mode)
             (ert-flow-headerline--apply (current-buffer)))))))))

;; Helper to clear caches (used by icon/control refresh)
(defun ert-flow-headerline-refresh ()
  "Clear header-line caches and force redisplay in panel buffers."
  (dolist (buf (buffer-list))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when (eq major-mode 'ert-flow-panel-mode)
          (setq-local ert-flow-headerline--cache-key nil)
          (setq-local ert-flow-headerline--cache-str nil)))))
  (force-mode-line-update t))

;; Apply to existing panel buffers on load
(dolist (buf (buffer-list))
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (when (eq major-mode 'ert-flow-panel-mode)
        (ert-flow-headerline--apply (current-buffer))))))

(provide 'ert-flow-headerline)
;;; ert-flow-headerline.el ends here
