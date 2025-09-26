;;; test-flow-headerline.el --- Header-line controls for test-flow  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Builds header-line content for test-flow panel buffers.
;; - Uses test-flow-view-controls to generate interactive segments
;; - Light caching keyed by UI-relevant inputs
;; - Feature flag to enable/disable header-line
;; - Applies/removes header-line for panel buffers

;;; Code:

(require 'cl-lib)
(require 'subr-x)

;; Soft deps (optional to avoid hard require cycles)
(require 'test-flow-view-controls nil t)
(require 'test-flow-controls-icons nil t)

;;;###autoload
(defgroup test-flow-headerline nil
  "Header-line controls for test-flow."
  :group 'test-flow)

(defcustom test-flow-view-headerline-enable t
  "When non-nil, show controls in the header-line of test-flow panel buffers."
  :type 'boolean :group 'test-flow-headerline)

;; Cache variables are buffer-local to each panel
(defvar-local test-flow-headerline--cache-key nil)
(defvar-local test-flow-headerline--cache-str nil)

(defvar test-flow--active-run-count 0)
(defvar test-flow--run-queue nil)

;; Глобальная карта для header-line: гасит нажатия и тянучки, а на отпускание
;; выполняет команду, помеченную свойством `test-flow-action' под курсором.
(defun test-flow-headerline--consume (e)
  "Поглотить событие мыши в header-line, не передавая его в буфер."
  (interactive "e")
  nil)

(defun test-flow-headerline--on-mouse (e)
  "Выполнить команду, помеченную на сегменте под курсором (test-flow-action)."
  (interactive "e")
  (let* ((pos (event-end e))
         (sp  (posn-string pos)))
    (when (consp sp)
      (let* ((str (car sp))
             (idx (cdr sp))
             (cmd (and (stringp str) (integerp idx)
                       (get-text-property idx 'test-flow-action str))))
        (when (and (symbolp cmd) (commandp cmd))
          ;; Выполним после отпускания, чтобы гарантированно не задеть буфер.
          (run-at-time 0 nil (lambda () (call-interactively cmd)))))))
  nil)

(defvar test-flow-headerline--global-map
  (let ((m (make-sparse-keymap)))
    ;; Поглотить нажатие и перетаскивание в header-line
    (define-key m [header-line down-mouse-1] #'test-flow-headerline--consume)
    (define-key m [header-line drag-mouse-1] #'test-flow-headerline--consume)
    ;; Выполнить команду на отпускание в header-line
    (define-key m [header-line mouse-1] #'test-flow-headerline--on-mouse)
    m)
  "Глобальная keymap для всей строки header-line (не перехватывает клики вне header-line).")

(defun test-flow-headerline--fallback-segments ()
  "Return a minimal textual controls list for header-line as a fallback."
  (let* ((watch-on (ignore-errors
                     (when (fboundp 'test-flow--find-panel-session)
                       (let ((s (test-flow--find-panel-session)))
                         (cond
                          ((and s (fboundp 'test-flow--get-watch-enabled))
                           (test-flow--get-watch-enabled s))
                          ((and s (fboundp 'test-flow--session-watch-enabled))
                           (test-flow--session-watch-enabled s))
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
                   (add-text-properties 0 len (list 'test-flow-action cmd) s))
                 ;; Не подсвечиваем ведущий пробел, чтобы не сливалось с соседями.
                 (when (> len 0)
                   (add-text-properties 0 (min 1 len) (list 'pointer 'arrow) s)
                   (if (symbolp cmd)
                       (add-text-properties 1 len (list 'pointer 'hand 'mouse-face 'highlight) s)
                     (add-text-properties 1 len (list 'pointer 'arrow) s)))
                 s))))
    (list
     (funcall mk "[Run]" #'test-flow-run "Run all tests (r)")
     (funcall gap)
     (funcall mk "[↻]" #'test-flow-run-failed "Run failed tests (f)")
     (funcall gap)
     (funcall mk (format "[Watch:%s]" (if watch-on "On" "Off"))
              #'test-flow-toggle-watch "Toggle watch (w)")
     (funcall gap)
     (funcall mk "[Copy]" #'test-flow-copy-failures "Copy failures (c)")
     (funcall gap)
     (funcall mk "[Clear]" #'test-flow-clear "Clear panel (x)")
     (funcall gap)
     (funcall mk "[Detect]" #'test-flow-detect-runner "Detect runner (d)")
     (funcall gap)
     (funcall mk "[Goto]" #'test-flow-goto-definition-at-point "Goto test definition (o)"))))

(defun test-flow-headerline-format ()
  "Return header-line content for test-flow panel buffers with caching."
  (when (eq major-mode 'test-flow-panel-mode)
    (let* ((style (and (boundp 'test-flow-toolbar-style)
                       test-flow-toolbar-style))
           (icons-on (and (fboundp 'test-flow-controls-icons-available-p)
                          (test-flow-controls-icons-available-p)))
           (watch-on (ignore-errors
                       (when (fboundp 'test-flow--find-panel-session)
                         (let ((s (test-flow--find-panel-session)))
                           (cond
                            ((and s (fboundp 'test-flow--get-watch-enabled))
                             (test-flow--get-watch-enabled s))
                            ((and s (fboundp 'test-flow--session-watch-enabled))
                             (test-flow--session-watch-enabled s))
                            (t nil))))))
           (log-on (and (boundp 'test-flow-log-enabled) test-flow-log-enabled))
           (active (and (boundp 'test-flow--active-run-count) test-flow--active-run-count))
           (queued (and (boundp 'test-flow--run-queue) (length test-flow--run-queue)))
           (key (list style icons-on (and watch-on t) (and log-on t) active queued)))
      (let* ((s
              (if (equal key test-flow-headerline--cache-key)
                  test-flow-headerline--cache-str
                (let* ((controls (or
                                  (ignore-errors
                                    (when (fboundp 'test-flow-view-controls-segments)
                                      ;; Лёгкое смещение для иконок в header-line
                                      (let ((test-flow-controls-icon-raise -0.08))
                                        (test-flow-view-controls-segments))))
                                  (test-flow-headerline--fallback-segments)))
                       (out (mapconcat #'identity controls "")))
                  (setq test-flow-headerline--cache-key key
                        test-flow-headerline--cache-str out)
                  out))))
        ;; Гарантируем наличие глобальной карты на всей строке (даже если в кэше старая строка).
        (when (stringp s)
          (add-text-properties 0 (length s)
                               (list 'keymap test-flow-headerline--global-map
                                     'local-map test-flow-headerline--global-map)
                               s))
        s))))

(defun test-flow-headerline--apply (buffer)
  "Apply or remove header-line in BUFFER according to feature flag."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (eq major-mode 'test-flow-panel-mode)
        (if test-flow-view-headerline-enable
            (setq header-line-format '((:eval (test-flow-headerline-format))))
          (when (equal header-line-format '((:eval (test-flow-headerline-format))))
            (setq header-line-format nil)))
        (ignore-errors
          (when (fboundp 'test-flow-view-controls--ensure-headerline-face)
            (test-flow-view-controls--ensure-headerline-face)))
        (force-mode-line-update t)))))

;; Watcher: toggling feature flag should apply/remove header-line everywhere.
(when (fboundp 'add-variable-watcher)
  (add-variable-watcher
   'test-flow-view-headerline-enable
   (lambda (&rest _)
     (dolist (buf (buffer-list))
       (when (buffer-live-p buf)
         (with-current-buffer buf
           (when (eq major-mode 'test-flow-panel-mode)
             (test-flow-headerline--apply (current-buffer)))))))))

;; Helper to clear caches (used by icon/control refresh)
(defun test-flow-headerline-refresh ()
  "Clear header-line caches and force redisplay in panel buffers."
  (dolist (buf (buffer-list))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when (eq major-mode 'test-flow-panel-mode)
          (setq-local test-flow-headerline--cache-key nil)
          (setq-local test-flow-headerline--cache-str nil)))))
  (force-mode-line-update t))

;; Apply to existing panel buffers on load
(dolist (buf (buffer-list))
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (when (eq major-mode 'test-flow-panel-mode)
        (test-flow-headerline--apply (current-buffer))))))

(provide 'test-flow-headerline)
;;; test-flow-headerline.el ends here
