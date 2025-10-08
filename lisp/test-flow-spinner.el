;;; test-flow-spinner.el --- Spinner & progress helpers for test-flow panel -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Lightweight spinner implementation and progress viewer used by test-flow.
;; - Show animated spinner and approximate percentage in panel while a run is active.
;; - Attach/detach to process lifecycle from test-runner.
;; - Provide `test-flow-show-progress' to open a live view of process output (bound to "$" in panel).
;;
;; This intentionally keeps a small API:
;; - test-flow-spinner-attach (sess proc)
;; - test-flow-spinner-detach (sess proc)
;; - test-flow-spinner-frame ()
;; - test-flow-spinner-percent-from-session (sess proc)
;; - test-flow-show-progress (interactive)

;;; Code:

(require 'subr-x)
(require 'test-flow-core)
(require 'test-flow-parse) ;; reuse parsing helpers for progress heuristics

(defcustom test-flow-spinner-frames
  '("⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏")
  "Frames used by test-flow spinner."
  :type '(repeat string)
  :group 'test-flow-core)

(defcustom test-flow-spinner-interval 0.12
  "Spinner animation interval in seconds."
  :type 'number
  :group 'test-flow-core)

(defcustom test-flow-spinner-degrade-threshold 0.25
  "If timer slips by more than this (seconds), stop animation and show static indicator."
  :type 'number
  :group 'test-flow-core)

;; Buffer-local spinner state variables (stored in panel buffer)
(defvar-local test-flow--spinner-timer nil)
(defvar-local test-flow--spinner-index 0)
(defvar-local test-flow--spinner-last-time 0.0)
(defvar-local test-flow--spinner-degraded nil)
;; Link to session/proc and state for light UI updates in panel
(defvar-local test-flow--spinner-sess nil)
(defvar-local test-flow--spinner-proc nil)
(defvar-local test-flow--spinner-rendered nil)

(defun test-flow-spinner--ensure-state ()
  "Ensure spinner buffer-local state is initialized."
  (unless (numberp test-flow--spinner-index)
    (setq test-flow--spinner-index 0))
  (unless (numberp test-flow--spinner-last-time)
    (setq test-flow--spinner-last-time (float-time)))
  (unless (booleanp test-flow--spinner-degraded)
    (setq test-flow--spinner-degraded nil)))

(defun test-flow-spinner-frame ()
  "Return current spinner frame string for the buffer (based on index)."
  (let ((frames (or test-flow-spinner-frames '("⠋"))))
    (if (and (numberp test-flow--spinner-index) (> (length frames) 0))
        (nth (mod test-flow--spinner-index (length frames)) frames)
      (car frames))))

(defun test-flow-spinner-percent-from-session (sess proc)
  "Estimate completion fraction [0..1] for SESS/PROC using available output.
Return nil if unknown, or a float 0..1."
  (ignore-errors
    (let* ((stdout-buf (process-get proc 'test-flow-stdout-buf))
           (raw (when (buffer-live-p stdout-buf)
                  (with-current-buffer stdout-buf (buffer-string))))
           ;; Use batch-pass1 heuristic: it extracts total/unexpected etc.
           (lines (when (stringp raw) (split-string raw "\n")))
           (pass1 (and lines (test-flow-parse--batch-pass1 lines)))
           (total (or (plist-get pass1 :total)
                      ;; try to infer from "passed X/Y" occurrences
                      (when (and (stringp raw)
                                 (string-match "passed[ \t]+\\([0-9]+\\)/\\([0-9]+\\)" raw))
                        (string-to-number (match-string 2 raw)))))
           (done (when pass1
                   ;; count names present in pass1's all-names, or count statuses seen
                   (let ((nmap (plist-get pass1 :name->status)))
                     (let ((cnt 0))
                       (maphash (lambda (_ v) (cl-incf cnt)) nmap)
                       cnt)))))
      (when (and (numberp total) (> total 0) (numberp done))
        (min 1.0 (/ (float done) (float total)))))))

;;; Spinner lifecycle: attach/detach called by the runner when process starts/stops

(defun test-flow-spinner--inline-refresh (buf)
  "Try to update spinner region in BUF without full re-render. Return t on success."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (let ((sess (and (boundp 'test-flow--spinner-sess) test-flow--spinner-sess))
            (proc (and (boundp 'test-flow--spinner-proc) test-flow--spinner-proc)))
        (when (and sess proc
                   (boundp 'test-flow--spinner-beg)
                   (boundp 'test-flow--spinner-end)
                   (markerp test-flow--spinner-beg)
                   (markerp test-flow--spinner-end)
                   (get-buffer-window buf 'visible))
          (let* ((frame (test-flow-spinner-frame))
                 (pct (test-flow-spinner-percent-from-session sess proc))
                 (pct-str (and (numberp pct) (format "%d%%" (truncate (* 100 pct)))))
                 (str (format "  %s %s\n" frame (or pct-str "Running..."))))
            (let ((inhibit-read-only t))
              (save-excursion
                (goto-char test-flow--spinner-beg)
                (delete-region test-flow--spinner-beg test-flow--spinner-end)
                (insert str)
                (set-marker test-flow--spinner-end (point))))
            t))))))

(defun test-flow-spinner--start-timer-for-buffer (buf)
  "Start or restart spinner timer attached to panel buffer BUF."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (when (timerp test-flow--spinner-timer)
        (cancel-timer test-flow--spinner-timer))
      (setq test-flow--spinner-index 0)
      (setq test-flow--spinner-last-time (float-time))
      (setq test-flow--spinner-degraded nil)
      (let ((interval (or test-flow-spinner-interval 0.12))
            (bb (current-buffer)))
        (setq test-flow--spinner-timer
              (run-at-time 0 interval
                           (lambda ()
                             (let ((bb2 (if (buffer-live-p bb) bb
                                          (get-buffer (or (and (boundp 'test-flow--panel-buffer-name) test-flow--panel-buffer-name)
                                                          "*test-flow*")))))
                               (when (buffer-live-p bb2)
                                 (with-current-buffer bb2
                                   (let* ((now (float-time))
                                          (dt (- now (or test-flow--spinner-last-time now)))
                                          (thr (or test-flow-spinner-degrade-threshold 0.25)))
                                     (setq test-flow--spinner-last-time now)
                                     (when (> dt (+ interval thr))
                                       (setq test-flow--spinner-degraded t)
                                       (when (timerp test-flow--spinner-timer)
                                         (cancel-timer test-flow--spinner-timer))
                                       (setq test-flow--spinner-timer nil))
                                     (unless test-flow--spinner-degraded
                                       (setq test-flow--spinner-index (1+ (or test-flow--spinner-index 0))))
                                     ;; Light inline update of the spinner line; avoid full re-render
                                     (ignore-errors
                                       (test-flow-spinner--inline-refresh bb2)))))))))))))

(defun test-flow-spinner--stop-timer-for-buffer (buf)
  "Stop spinner timer for panel buffer BUF and reset state."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (when (timerp test-flow--spinner-timer)
        (cancel-timer test-flow--spinner-timer))
      (setq test-flow--spinner-timer nil)
      (setq test-flow--spinner-index 0)
      (setq test-flow--spinner-last-time 0.0)
      (setq test-flow--spinner-degraded nil))))

;;; Public attach/detach used by runner

(defun test-flow-spinner-attach (sess proc)
  "Attach spinner to session SESS / process PROC.
Start spinner timer in the session panel buffer so UI will show animation."
  (when (and sess proc)
    (let* ((root (and (fboundp 'test-flow--session-root) (test-flow--session-root sess)))
           (bufname (and (fboundp 'test-flow--session-panel-name)
                         (test-flow--session-panel-name root)))
           (buf (get-buffer-create bufname)))
      (with-current-buffer buf
        (test-flow-spinner--ensure-state)
        (setq-local test-flow--spinner-sess sess)
        (setq-local test-flow--spinner-proc proc)
        (setq-local test-flow--spinner-rendered nil))
      (test-flow-spinner--start-timer-for-buffer buf)
      ;; One initial render to create spinner region; subsequent updates are inline
      (with-current-buffer buf
        (when (fboundp 'test-flow--render)
          (let ((test-flow--panel-buffer-name (buffer-name)))
            (test-flow--render))
          (setq-local test-flow--spinner-rendered t))))))

(defun test-flow-spinner-detach (sess proc)
  "Detach spinner from session SESS / process PROC and stop timer."
  (when sess
    (let* ((root (and (fboundp 'test-flow--session-root) (test-flow--session-root sess)))
           (bufname (and (fboundp 'test-flow--session-panel-name)
                         (test-flow--session-panel-name root)))
           (buf (get-buffer bufname)))
      (when (buffer-live-p buf)
        (test-flow-spinner--stop-timer-for-buffer buf)
        (with-current-buffer buf
          ;; clear links/markers
          (setq test-flow--spinner-sess nil)
          (setq test-flow--spinner-proc nil)
          (setq test-flow--spinner-rendered nil)
          (setq test-flow--spinner-beg nil)
          (setq test-flow--spinner-end nil)
          ;; force one final render to show completed state
          (when (fboundp 'test-flow--render)
            (let ((test-flow--panel-buffer-name bufname))
              (test-flow--render))))))))

;;; Live progress viewer

(defun test-flow--progress-buffer-name (root)
  "Return progress buffer name for project ROOT."
  (format "*test-flow: progress %s*" (file-name-nondirectory (directory-file-name root))))

(defvar-local test-flow--progress-poller-timer nil)

(defun test-flow--progress--current-session-proc ()
  "Return plist (:sess :proc :root) for the current panel or project."
  (let* ((sess (or (and (eq major-mode 'test-flow-panel-mode) (test-flow--find-panel-session))
                   (test-flow--get-session (test-flow--project-root))))
         (proc (and sess (test-flow--get-process sess)))
         (root (and sess (test-flow--session-root sess))))
    (list :sess sess :proc proc :root root)))

(defun test-flow--progress--render-into (tgt-buf src proc)
  "Render header and SRC contents into TGT-BUF for PROC."
  (when (and (buffer-live-p tgt-buf))
    (with-current-buffer tgt-buf
      (let ((inhibit-read-only t)
            (p (point)))
        (erase-buffer)
        (insert (format "Live output for: %s\n\n" (or (process-get proc 'test-flow-label) "test-flow run")))
        (when (buffer-live-p src)
          (insert (with-current-buffer src (buffer-string))))
        (goto-char (min p (point-max))))
      (unless (derived-mode-p 'special-mode)
        (special-mode)))))

(defun test-flow--progress--start-poller (tgt-buf src proc)
  "Start/replace polling timer to keep TGT-BUF in sync with SRC for PROC."
  (when (timerp (ignore-errors (buffer-local-value 'test-flow--progress-poller-timer tgt-buf)))
    (cancel-timer (buffer-local-value 'test-flow--progress-poller-timer tgt-buf)))
  (with-current-buffer tgt-buf
    (setq-local test-flow--progress-poller-timer
                (run-at-time 0.2 0.25
                             (lambda ()
                               (when (buffer-live-p tgt-buf)
                                 (test-flow--progress--render-into tgt-buf src proc)
                                 (unless (process-live-p proc)
                                   (when (timerp test-flow--progress-poller-timer)
                                     (cancel-timer test-flow--progress-poller-timer)
                                     (setq-local test-flow--progress-poller-timer nil)
                                     (message "test-flow: process finished"))))))))
  (with-current-buffer tgt-buf
    (add-hook 'kill-buffer-hook
              (lambda ()
                (when (timerp test-flow--progress-poller-timer)
                  (cancel-timer test-flow--progress-poller-timer)
                  (setq-local test-flow--progress-poller-timer nil)))
              nil t)))

(defun test-flow-show-progress ()
  "Show live progress/output for current panel's session (interactive).
Opens a side buffer that tails the process stdout/stderr if a run is active."
  (interactive)
  (pcase-let* ((plist (test-flow--progress--current-session-proc))
               (sess (plist-get plist :sess))
               (proc (plist-get plist :proc))
               (root (plist-get plist :root)))
    (unless (and sess proc (process-live-p proc))
      (user-error "No active run for current session"))
    (let* ((src (or (process-get proc 'test-flow-stdout-buf)
                    (get-buffer (process-buffer proc))))
           (name (test-flow--progress-buffer-name root))
           (buf  (get-buffer-create name)))
      (test-flow--progress--render-into buf src proc)
      (let ((win (display-buffer-in-side-window buf `((side . bottom) (window-height . 0.4)))))
        (select-window win))
      (test-flow--progress--start-poller buf src proc))))


(provide 'test-flow-spinner)
;;; test-flow-spinner.el ends here
