;;; test-flow-coverage.el --- LCOV coverage analyzer and UI helpers -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Minimal LCOV coverage analyzer for Emacs Lisp projects (language-agnostic LCOV reader).
;; - Parse coverage/lcov.info (or custom paths)
;; - Store per-session coverage summary and per-file stats
;; - Insert a Coverage block into the test-flow panel (if requested)
;; - Provide simple line overlays for missed lines in current buffer
;;
;; This module is optional. test-flow detects and calls it dynamically when present.
;;
;; Public commands:
;; - test-flow-coverage-load        — load/parse lcov.info into current session
;; - test-flow-coverage-clear       — clear coverage data and overlays
;; - test-flow-coverage-toggle-overlays — toggle missed-lines overlays for current buffer
;; - test-flow-coverage-next-missed — jump to next missed line in current buffer
;;
;; Panel integration:
;; - test-flow-coverage--insert-panel-block (called by test-flow when available)

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

;; Soft deps: avoid hard requires to keep module optional
(declare-function test-flow--project-root "test-flow" ())
(declare-function test-flow--get-session "test-flow" (&optional root))
(declare-function test-flow--session-panel-name "test-flow" (root))
(declare-function test-flow--render "test-flow" ())
(declare-function test-flow--session-list "test-flow" ())
(declare-function test-flow--session-root "test-flow" (sess))

;;;###autoload
(defgroup test-flow-coverage nil
  "Coverage analyzer and UI helpers for test-flow."
  :group 'test-flow)

(defcustom test-flow-coverage-auto-load t
  "If non-nil, try to auto-load LCOV coverage after each test run completes."
  :type 'boolean
  :group 'test-flow-coverage)

(defcustom test-flow-coverage-lcov-path '("coverage/lcov.info" "lcov.info")
  "Candidate paths (relative to project root) for LCOV file discovery."
  :type '(repeat string)
  :group 'test-flow-coverage)

(defcustom test-flow-coverage-max-files-in-panel 20
  "How many files to list in the panel Coverage block."
  :type 'integer
  :group 'test-flow-coverage)

(defface test-flow-coverage-missed
  '((t :background "#3a1e1e"))
  "Face for missed lines overlays."
  :group 'test-flow-coverage)

(defface test-flow-coverage-hit
  '((t :background "#1e3a1e"))
  "Face for hit lines overlays (unused by default)."
  :group 'test-flow-coverage)

(defvar-local test-flow-coverage--overlays nil
  "List of overlays created by coverage in the current buffer.")

;; Session storage helpers (stash in session config to avoid struct churn)
(defun test-flow-coverage--sess-get (sess key)
  (when (and sess (fboundp 'test-flow--conf))
    (funcall 'test-flow--conf sess key nil)))

(defun test-flow-coverage--sess-set (sess key value)
  (when (and sess (fboundp 'test-flow--set-conf))
    (funcall 'test-flow--set-conf sess key value)
    value))

(defun test-flow-coverage--canon (file)
  "Return canonical absolute path for FILE (tolerates non-existing files)."
  (let ((abs (expand-file-name file)))
    (or (ignore-errors (file-truename abs)) abs)))

;;;; LCOV parser

(defun test-flow-coverage--parse-lcov-lines (lines)
  "Parse LCOV LINES (list of strings). Return cons (SUMMARY . FILES).
FILES is an alist of (FILE . PLIST) with keys:
  :lines-found :lines-hit :percent :missed-lines (list of numbers) :hits (hash lno→count)."
  (let ((files '())
        (cur nil))
    (dolist (ln lines)
      (cond
       ;; Start of file
       ((string-prefix-p "SF:" ln)
        (let* ((path (string-trim (substring ln 3)))
               (canon (test-flow-coverage--canon path))
               (entry (list :file canon
                            :hits (make-hash-table :test 'eql)
                            :lines-found 0
                            :lines-hit 0
                            :missed-lines nil)))
          (setq cur (cons canon entry))
          (push cur files)))
       ;; DA:<line>,<hits>
       ((and cur (string-prefix-p "DA:" ln))
        (let* ((rest (substring ln 3))
               (comma (cl-position ?, rest))
               (lno (and comma (string-to-number (substring rest 0 comma))))
               (cnt (and comma (string-to-number (substring rest (1+ comma)))))
               (plist (cdr cur))
               (ht (plist-get plist :hits)))
          (when (and (integerp lno) (>= lno 1))
            (puthash lno cnt ht)
            (setf (plist-get plist :lines-found) (1+ (plist-get plist :lines-found)))
            (if (and (numberp cnt) (> cnt 0))
                (setf (plist-get plist :lines-hit) (1+ (plist-get plist :lines-hit)))
              (push lno (plist-get plist :missed-lines))))))
       ;; End of record
       ((string= ln "end_of_record")
        (setq cur nil))
       ;; Ignore others (LF/LH present are ignored; we recompute)
       (t nil)))
    (setq files (nreverse files))
    ;; finalize per-file percent and global summary
    (let ((tot-lf 0) (tot-lh 0))
      (dolist (cell files)
        (let* ((plist (cdr cell))
               (lf (plist-get plist :lines-found))
               (lh (plist-get plist :lines-hit))
               (pct (if (> lf 0) (* 100.0 (/ (float lh) (float lf))) 0.0)))
          (setf (plist-get plist :percent) pct)
          (setq tot-lf (+ tot-lf lf)
                tot-lh (+ tot-lh lh))))
      (let ((summary `((lines-found . ,tot-lf)
                       (lines-hit   . ,tot-lh)
                       (percent     . ,(if (> tot-lf 0) (* 100.0 (/ (float tot-lh) (float tot-lf))) 0.0)))))
        (cons summary files)))))

(defun test-flow-coverage--parse-lcov-file (path)
  "Parse LCOV file PATH, returning cons (SUMMARY . FILES)."
  (with-temp-buffer
    (insert-file-contents path)
    (let* ((txt (buffer-string))
           (lines (split-string txt "\n" t)))
      (test-flow-coverage--parse-lcov-lines lines))))

;;;; Loading, panel block, overlays

;;;###autoload
(defun test-flow-coverage-load (&optional quiet)
  "Load LCOV coverage for the current project session. With QUIET, suppress messages."
  (interactive)
  (let* ((root (if (fboundp 'test-flow--project-root)
                   (test-flow--project-root)
                 default-directory))
         (sess (and (fboundp 'test-flow--get-session) (test-flow--get-session root)))
         (cands test-flow-coverage-lcov-path)
         (found nil))
    (dolist (rel cands)
      (let* ((abs (expand-file-name rel root)))
        (when (and (not found) (file-exists-p abs))
          (setq found abs))))
    (if (not found)
        (unless quiet (message "test-flow: coverage not found (tried: %s)" (mapconcat #'identity test-flow-coverage-lcov-path ", ")))
      (pcase-let* ((`(,summary . ,files) (test-flow-coverage--parse-lcov-file found)))
        (when sess
          (test-flow-coverage--sess-set sess 'coverage-summary summary)
          (test-flow-coverage--sess-set sess 'coverage-files files))
        (unless quiet
          (let ((lf (alist-get 'lines-found summary))
                (lh (alist-get 'lines-hit summary))
                (pct (alist-get 'percent summary)))
            (message "test-flow: coverage loaded: %.1f%% (%d/%d) from %s" pct lh lf (file-relative-name found root))))
        ;; Re-render panel if it exists
        (when (and (fboundp 'test-flow--session-panel-name)
                   (fboundp 'test-flow--render))
          (let ((bufname (test-flow--session-panel-name root)))
            (when (get-buffer bufname)
              (let ((test-flow--panel-buffer-name bufname))
                (test-flow--render)))))))))

;;;###autoload
(defun test-flow-coverage-clear ()
  "Clear coverage data for the current session and remove overlays in visible buffers."
  (interactive)
  (let* ((root (if (fboundp 'test-flow--project-root)
                   (test-flow--project-root)
                 default-directory))
         (sess (and (fboundp 'test-flow--get-session) (test-flow--get-session root))))
    (when sess
      (test-flow-coverage--sess-set sess 'coverage-summary nil)
      (test-flow-coverage--sess-set sess 'coverage-files nil))
    (dolist (buf (buffer-list))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (when test-flow-coverage--overlays
            (mapc #'delete-overlay test-flow-coverage--overlays)
            (setq test-flow-coverage--overlays nil)))))
    (message "test-flow: coverage cleared")
    ;; Rerender panel if present
    (when (and (fboundp 'test-flow--session-panel-name)
               (fboundp 'test-flow--render))
      (let ((bufname (test-flow--session-panel-name root)))
        (when (get-buffer bufname)
          (let ((test-flow--panel-buffer-name bufname))
            (test-flow--render)))))))

(defun test-flow-coverage--files-for-sess (sess)
  "Return per-file coverage alist for SESS or nil."
  (test-flow-coverage--sess-get sess 'coverage-files))

(defun test-flow-coverage--summary-for-sess (sess)
  "Return coverage summary alist for SESS or nil."
  (test-flow-coverage--sess-get sess 'coverage-summary))

(defun test-flow-coverage--find-meta-for-current-buffer (sess)
  "Return coverage plist for current buffer's file in SESS, or nil."
  (let ((file (buffer-file-name)))
    (when (and file sess)
      (let* ((canon (test-flow-coverage--canon file))
             (files (test-flow-coverage--files-for-sess sess)))
        (cdr (assoc canon files))))))

(defun test-flow-coverage--pick-session-for-file (file)
  "Pick best test-flow session for FILE by longest matching root prefix."
  (let* ((abs (test-flow-coverage--canon file))
         (best nil)
         (best-len -1))
    (when (fboundp 'test-flow--session-list)
      (dolist (s (test-flow--session-list))
        (let* ((root (and (fboundp 'test-flow--session-root)
                          (test-flow--session-root s)))
               (root* (and root (file-name-as-directory (expand-file-name root)))))
          (when (and (stringp root*) (string-prefix-p root* abs))
            (let ((len (length root*)))
              (when (> len best-len)
                (setq best s best-len len)))))))
    best))

;;;###autoload
(defun test-flow-coverage-overlays-clear ()
  "Remove coverage overlays in the current buffer."
  (interactive)
  (when test-flow-coverage--overlays
    (mapc #'delete-overlay test-flow-coverage--overlays)
    (setq test-flow-coverage--overlays nil)))

;;;###autoload
(defun test-flow-coverage-overlays-apply ()
  "Apply missed-lines coverage overlays to the current buffer (if coverage is loaded)."
  (interactive)
  (test-flow-coverage-overlays-clear)
  (let* ((file (buffer-file-name))
         (sess (or (and file (test-flow-coverage--pick-session-for-file file))
                   (and (fboundp 'test-flow--get-session)
                        (funcall 'test-flow--get-session
                                 (if (fboundp 'test-flow--project-root)
                                     (test-flow--project-root)
                                   default-directory)))))
         (meta (and sess (test-flow-coverage--find-meta-for-current-buffer sess))))
    (when meta
      (save-excursion
        (dolist (ln (sort (copy-sequence (plist-get meta :missed-lines)) #'<))
          (goto-char (point-min))
          (forward-line (1- ln))
          (let ((ov (make-overlay (line-beginning-position) (line-end-position))))
            (overlay-put ov 'face 'test-flow-coverage-missed)
            (push ov test-flow-coverage--overlays)))))))

;;;###autoload
(defun test-flow-coverage-toggle-overlays ()
  "Toggle missed-lines coverage overlays in the current buffer."
  (interactive)
  (if test-flow-coverage--overlays
      (test-flow-coverage-overlays-clear)
    (test-flow-coverage-overlays-apply)))

;;;###autoload
(defun test-flow-coverage-next-missed ()
  "Jump to the next missed line according to loaded coverage."
  (interactive)
  (let* ((sess (and (fboundp 'test-flow--get-session)
                    (funcall 'test-flow--get-session (if (fboundp 'test-flow--project-root)
                                                         (test-flow--project-root)
                                                       default-directory))))
         (meta (test-flow-coverage--find-meta-for-current-buffer sess))
         (ln (line-number-at-pos))
         (cands (and meta (sort (copy-sequence (plist-get meta :missed-lines)) #'<)))
         (next (seq-find (lambda (x) (> x ln)) cands)))
    (if next
        (progn (goto-char (point-min)) (forward-line (1- next)) (message "goto missed: %d" next))
      (message "No next missed line"))))

(defun test-flow-coverage--shorten-path (root file)
  "Return FILE relative to ROOT when possible."
  (let* ((root* (file-name-as-directory (expand-file-name root))))
    (if (string-prefix-p root* file)
        (file-relative-name file root*)
      file)))

(defun test-flow-coverage--insert-panel-block (sess)
  "Insert Coverage block into the current panel buffer for SESS."
  (let* ((sum (test-flow-coverage--summary-for-sess sess))
         (files (test-flow-coverage--files-for-sess sess))
         (root (and sess (if (fboundp 'test-flow--project-root) (test-flow--project-root) default-directory))))
    (insert (propertize "Coverage\n" 'face 'bold))
    (if (not sum)
        (insert "  No coverage data. M-x test-flow-coverage-load\n\n")
      (let* ((lf (alist-get 'lines-found sum))
             (lh (alist-get 'lines-hit sum))
             (pct (alist-get 'percent sum)))
        (cond
         ;; Пустой LCOV (нет DA записей) — подсказка как получить реальное покрытие
         ((or (null lf) (= lf 0))
          (insert "  No executable lines (empty LCOV). Generate real coverage via Undercover/elisp-coverage.\n\n"))
         (t
          (insert (format "  Total: %.1f%% (%d/%d)\n" pct lh lf))
          (let* ((sorted (seq-take
                          (sort (copy-sequence files)
                                (lambda (a b)
                                  (< (plist-get (cdr a) :percent)
                                     (plist-get (cdr b) :percent))))
                          test-flow-coverage-max-files-in-panel)))
            (dolist (cell sorted)
              (let* ((file (car cell))
                     (meta (cdr cell))
                     (fpct (plist-get meta :percent))
                     (lf* (plist-get meta :lines-found))
                     (lh* (plist-get meta :lines-hit))
                     (miss (plist-get meta :missed-lines))
                     (label (format "  %-6.1f%% %s (%d/%d)%s\n"
                                    fpct
                                    (test-flow-coverage--shorten-path root file)
                                    lh* lf*
                                    (if miss (format "  [missed:%d]" (length miss)) ""))))
                (insert-text-button
                 label
                 'follow-link t
                 'help-echo "Open file (mouse-1) and jump to first missed line"
                 'action (lambda (_)
                           (find-file file)
                           (when miss
                             (goto-char (point-min))
                             (forward-line (1- (apply #'min miss)))))
                 'face 'default))))
          (insert "\n")))))))

(provide 'test-flow-coverage)
;;; test-flow-coverage.el ends here
