;;; test-flow-render.el --- Pure rendering helpers for test-flow  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Standalone rendering helpers (icons, faces, counters).
;; Initially delegates to monolith internals; real logic can be moved here later.

;;; Code:

(require 'cl-lib)
(eval-when-compile (require 'subr-x))

;; Forward declarations (quiet the byte-compiler and avoid hard deps)
(declare-function test-flow--apply-panel-filters "test-flow-panel" (results))
(defvar test-flow--panel-buffer-name nil)
(defvar test-flow--panel-status-folded t)
(defvar test-flow--panel-status-initialized nil)
(defvar test-flow--restore-point-suite nil)
;; Spinner update region (one line) for light updates without full re-render
(defvar-local test-flow--spinner-beg nil)
(defvar-local test-flow--spinner-end nil)

;;;###autoload
(defgroup test-flow-render nil
  "Rendering utilities for test-flow."
  :group 'test-flow)

;; Faces used by renderer (centralized here so UI modules can rely on them)
(defface test-flow-face-pass
  '((t :foreground "SpringGreen3" :weight bold))
  "Face for passed test icons."
  :group 'test-flow-render)

(defface test-flow-face-fail
  '((t :foreground "red3" :weight bold))
  "Face for failed test icons."
  :group 'test-flow-render)

(defface test-flow-face-error
  '((t :foreground "orange red" :weight bold))
  "Face for error test icons."
  :group 'test-flow-render)

(defface test-flow-face-skip
  '((t :inherit shadow))
  "Face for skipped/xfail test icons."
  :group 'test-flow-render)

(defun test-flow-render--require-monolith ()
  (require 'test-flow nil 'noerror))

;;;###autoload
(defun test-flow-render-status-icon (status)
  "Return a short icon string for STATUS."
  (let* ((style (and (boundp 'test-flow-toolbar-style) test-flow-toolbar-style))
         (icons-ok (and (memq style '(auto icons))
                        (featurep 'all-the-icons)
                        (display-graphic-p)
                        (find-font (font-spec :family "Material Icons"))
                        (fboundp 'all-the-icons-material))))
    (if icons-ok
        (pcase status
          ('pass  (all-the-icons-material "check_circle"))
          ('fail  (all-the-icons-material "cancel"))
          ('error (all-the-icons-material "error"))
          ('skip  (all-the-icons-material "remove_circle_outline"))
          ('xfail (all-the-icons-material "remove_circle_outline"))
          (_      (all-the-icons-material "help")))
      (pcase status
        ('pass "✓") ('fail "✕") ('error "!") ('skip "∼") ('xfail "∼") (_ "?")))))

;;;###autoload
(defun test-flow-render-status-face (status)
  "Return a face symbol for STATUS."
  (pcase status
    ('pass  'test-flow-face-pass)
    ('fail  'test-flow-face-fail)
    ('error 'test-flow-face-error)
    ('skip  'test-flow-face-skip)
    ('xfail 'test-flow-face-skip)
    (_ 'default)))

;;;###autoload
(defun test-flow-render-format-counters (summary results)
  "Return colored counters string: \"N (P:x F:y E:z S:s U:u)\".
Zero values are shown with a gray face; positive values use bright faces."
  (let* ((total (or (alist-get 'total summary) (length results)))
         (p (or (alist-get 'passed summary)
                (cl-count-if (lambda (r) (eq (plist-get r :status) 'pass)) results)))
         (f (or (alist-get 'failed summary)
                (cl-count-if (lambda (r) (eq (plist-get r :status) 'fail)) results)))
         (e (or (alist-get 'error summary)
                (cl-count-if (lambda (r) (eq (plist-get r :status) 'error)) results)))
         (s (or (alist-get 'skipped summary)
                (cl-count-if (lambda (r) (memq (plist-get r :status) '(skip xfail))) results)))
         (u (or (alist-get 'unexpected summary)
                (cl-count-if (lambda (r) (memq (plist-get r :status) '(fail error))) results)))
         (p0 (or p 0))
         (f0 (or f 0))
         (e0 (or e 0))
         (s0 (or s 0))
         (u0 (or u 0))
         (p-face (if (> p0 0) 'test-flow-face-pass 'shadow))
         (f-face (if (> f0 0) 'test-flow-face-fail 'shadow))
         (e-face (if (> e0 0) 'test-flow-face-error 'shadow))
         (s-face (if (> s0 0) 'test-flow-face-skip 'shadow))
         (u-face (if (> u0 0) 'test-flow-face-fail 'shadow)))
    (concat
     (format "%d (" (or total 0))
     (propertize (format "P:%d" p0) 'face p-face)
     " "
     (propertize (format "F:%d" f0) 'face f-face)
     " "
     (propertize (format "E:%d" e0) 'face e-face)
     " "
     (propertize (format "S:%d" s0) 'face s-face)
     " "
     (propertize (format "U:%d" u0) 'face u-face)
     ")")))

;; ---------------------------------------------------------------------------
;; More rendering helpers migrated from monolith
;; ---------------------------------------------------------------------------

;; We reference a few monolith internals; declare to keep byte-compiler calm.
(declare-function test-flow--ensure-fold-table "test-flow-panel" ())
;; (declare-function test-flow--render "test-flow-render" ())
(declare-function test-flow--log "test-flow-core" (fmt &rest args))

;;;###autoload
(defun test-flow-render-group-results (results)
  "Return alist (SUITE . LIST-OF-RESULTS) from RESULTS."
  (let ((ht (make-hash-table :test 'equal))
        acc)
    (dolist (r results)
      (let* ((suite (or (plist-get r :suite) ""))
             (bucket (gethash suite ht)))
        (puthash suite (cons r bucket) ht)))
    (maphash (lambda (k v) (push (cons k (nreverse v)) acc)) ht)
    (sort acc (lambda (a b) (string< (car a) (car b))))))

;;;###autoload
(defun test-flow-render-insert-test-line (r)
  "Insert single test line for result plist R with properties."
  (let* ((st (plist-get r :status))
         (nm (plist-get r :name))
         (suite (or (plist-get r :suite) ""))
         (strip-pref
          (cond
           ((and (stringp nm)
                 (stringp suite)
                 (> (length suite) 0)
                 (string-prefix-p (concat suite "/") nm))
            (concat suite "/"))
           ((and (stringp nm)
                 (boundp 'test-flow-display-strip-prefixes)
                 (listp test-flow-display-strip-prefixes))
            (seq-find (lambda (p) (and (stringp p) (string-prefix-p p nm)))
                      test-flow-display-strip-prefixes))
           (t nil)))
         (display-nm (if (and strip-pref (stringp nm))
                         (substring nm (length strip-pref))
                       nm))
         (visual-name
          (if strip-pref
              (concat (propertize strip-pref 'invisible 'test-flow-hide) (or display-nm nm))
            (or display-nm nm)))
         (icon (if (and (boundp 'test-flow-icons) test-flow-icons)
                   (test-flow-render-status-icon st)
                 ""))
         (face (test-flow-render-status-face st))
         (icon-face (and (> (length icon) 0) (get-text-property 0 'face icon)))
         (combined-face (if icon-face (list icon-face face) face))
         (icon-prop (if (> (length icon) 0)
                        (propertize icon 'face combined-face)
                      ""))
         (line (format "  %s %s\n" icon-prop visual-name)))
    (insert (propertize line 'test-flow--result r))))

;;;###autoload
(defun test-flow-render-suite-aggregate (results)
  "Return aggregate status symbol for RESULTS: all-pass, all-fail, mixed, skipped-only, or empty."
  (let* ((p (cl-count-if (lambda (r) (eq (plist-get r :status) 'pass)) results))
         (f (cl-count-if (lambda (r) (eq (plist-get r :status) 'fail)) results))
         (e (cl-count-if (lambda (r) (eq (plist-get r :status) 'error)) results))
         (s (cl-count-if (lambda (r) (memq (plist-get r :status) '(skip xfail))) results)))
    (cond
     ((and (= p 0) (= f 0) (= e 0) (= s 0)) 'empty)
     ((and (> p 0) (= f 0) (= e 0)) 'all-pass)
     ((and (= p 0) (> (+ f e) 0)) 'all-fail)
     ((and (= p 0) (= (+ f e) 0) (> s 0)) 'skipped-only)
     (t 'mixed))))

;;;###autoload
(defun test-flow-render-suite-icon (agg)
  "Return colored folder icon string for AGG aggregate status (cached)."
  ;; Lazy-init cache
  (defvar test-flow-render--suite-icon-cache nil)
  (unless (hash-table-p test-flow-render--suite-icon-cache)
    (setq test-flow-render--suite-icon-cache (make-hash-table :test 'equal)))
  (let* ((color (pcase agg
                  ('all-pass "SpringGreen3")
                  ('all-fail "Red3")
                  ('mixed "DarkOrange2")
                  ('skipped-only "gray60")
                  (_ "gray60")))
         (face `(:foreground ,color))
         (gfx (and (featurep 'all-the-icons) (display-graphic-p)))
         (cache-key (list agg gfx)))
    (or (gethash cache-key test-flow-render--suite-icon-cache)
        (let* ((icon
                (cond
                 ((and gfx
                       (fboundp 'all-the-icons-material)
                       (find-font (font-spec :family "Material Icons")))
                  (all-the-icons-material "folder" :v-adjust 0.0 :height 1.0))
                 ((char-displayable-p ?📁) "📁")
                 (t "[+]")))
               (out
                (cond
                 ((not (stringp icon)) icon)
                 (t
                  (let* ((icon-face (get-text-property 0 'face icon))
                         (combined (if icon-face (list icon-face face) face)))
                    (propertize icon 'face combined))))))
          (puthash cache-key out test-flow-render--suite-icon-cache)
          out))))

;;;###autoload
(defun test-flow-render-insert-suite (suite results)
  "Insert a SUITE heading and its RESULTS. Handles fold state and click action."
  (when (fboundp 'test-flow--ensure-fold-table)
    (test-flow--ensure-fold-table))
  (let* ((agg (test-flow-render-suite-aggregate results))
         (present (let ((marker '#:no))
                    (and (boundp 'test-flow--folded-suites)
                         (hash-table-p test-flow--folded-suites)
                         (not (eq (gethash suite test-flow--folded-suites marker) marker)))))
         (_init (unless present
                  (when (and (boundp 'test-flow--folded-suites)
                             (eq agg 'all-pass))
                    (puthash suite t test-flow--folded-suites))))
         (folded (and (boundp 'test-flow--folded-suites)
                      (gethash suite test-flow--folded-suites))))
    (when (and folded (not (eq agg 'all-pass)))
      (puthash suite nil test-flow--folded-suites)
      (setq folded nil)
      (when (fboundp 'test-flow--log)
        (test-flow--log "suite: auto-unfold due to non-pass aggregate → %s" suite)))
    (let* ((arrow (if folded "▸" "▾"))
           (icon (test-flow-render-suite-icon agg))
           (name suite)
           (s (concat arrow " " icon " " name "\n"))
           (arrow-len (length arrow))
           (icon-len (length icon))
           (name-start (+ arrow-len 1 icon-len 1))
           (name-end (1- (length s))))
      (add-text-properties 0 arrow-len '(face bold) s)
      (when (> name-end name-start)
        (add-text-properties name-start name-end '(face bold) s)
        (add-text-properties name-start name-end '(mouse-face highlight pointer hand) s))
      (insert-text-button
       s
       'face '(:underline nil)
       'mouse-face nil
       'follow-link t
       'help-echo "Toggle group (mouse-1, TAB)"
       'keymap (and (boundp 'test-flow--suite-button-map) test-flow--suite-button-map)
       'test-flow--suite suite
       'action #'test-flow-render--suite-button-action)
      (unless folded
        (dolist (r results)
          (test-flow-render-insert-test-line r))))))

;;;###autoload
(defun test-flow-render-status-line-icon (key &optional state)
  "Return icon for Status line KEY. Optional STATE for toggles like watch."
  (let ((gui (and (featurep 'all-the-icons)
                  (display-graphic-p)
                  (find-font (font-spec :family "Material Icons")))))
    (cond
     (gui
      (pcase key
        ('counters (all-the-icons-material "subject" :height 1.0 :v-adjust 0.02
                                           :face '(:foreground "LightSkyBlue3")))
        ('duration (all-the-icons-material "timer" :height 1.0 :v-adjust 0.02
                                           :face '(:foreground "gray70")))
        ('proc     (all-the-icons-material "autorenew" :height 1.0 :v-adjust 0.02
                                           :face '(:foreground "MediumPurple3")))
        ('project  (all-the-icons-material "folder_open" :height 1.0 :v-adjust 0.02
                                           :face '(:foreground "SteelBlue3")))
        ('runner   (all-the-icons-material "launch" :height 1.0 :v-adjust 0.02
                                           :face '(:foreground "Gold3")))
        ('mode     (all-the-icons-material "sync" :height 1.0 :v-adjust 0.02
                                           :face '(:foreground "DarkOrange2")))
        ('watch    (all-the-icons-material (if (eq state 'on) "visibility" "visibility_off")
                                           :height 1.0 :v-adjust 0.02
                                           :face (if (eq state 'on)
                                                     '(:foreground "DeepSkyBlue3")
                                                   '(:foreground "gray60"))))
        ('parser   (all-the-icons-material "extension" :height 1.0 :v-adjust 0.02
                                           :face '(:foreground "SlateGray3")))
        (_ "")))
     (t
      (pcase key
        ('counters (if (char-displayable-p ?📊) "📊" "Σ"))
        ('duration (if (char-displayable-p ?⏱) "⏱" "T"))
        ('proc     (if (char-displayable-p ?⚙) "⚙" "P"))
        ('project  (if (char-displayable-p ?📁) "📁" "D"))
        ('runner   (if (char-displayable-p ?🚀) "🚀" "R"))
        ('mode     (if (char-displayable-p ?⟳) "⟳" "M"))
        ('watch    (if (eq state 'on)
                       (if (char-displayable-p ?👁) "👁" "W")
                     (if (char-displayable-p ?🙈) "🙈" "w")))
        ('parser   (if (char-displayable-p ?🧩) "🧩" "X"))
        (_ ""))))))

;; ---------------------------------------------------------------------------
;; Render pipeline (migrated from monolith; called via defalias)
;; ---------------------------------------------------------------------------

;; Correct, single set of pipeline functions lives below (after duplicates removal).

;; Correct, single set of pipeline functions (override any earlier copies)

;; Declarations for monolith helpers used here
(declare-function test-flow--find-panel-session "test-flow-panel" ())
(declare-function test-flow--get-last-summary "test-flow-core" (sess))
(declare-function test-flow--get-last-results "test-flow-core" (sess))
(declare-function test-flow--get-process "test-flow-core" (sess))
(declare-function test-flow--conf "test-flow-core" (sess key default))
(declare-function test-flow--session-root "test-flow-core" (sess))
(declare-function test-flow--get-last-stderr-output "test-flow-core" (sess))
(declare-function test-flow--summary-counters "test-flow-panel" (sum results))
(declare-function test-flow--insert-status-block "test-flow-panel" (sess sum results))
(declare-function test-flow-coverage--insert-panel-block "test-flow-coverage" (sess))
(declare-function test-flow--goto-suite-heading "test-flow-panel" (suite))
;; (declare-function test-flow--render "test-flow-render" ())

(defun test-flow-render-render-context ()
  "Collect session context for rendering.
Returns plist: (:sess :sum :results :proc) and emits diagnostic logs."
  (let* ((sess (test-flow--find-panel-session))
         (sum (and sess (test-flow--get-last-summary sess)))
         (results (and sess (test-flow--get-last-results sess)))
         (proc (and sess (test-flow--get-process sess))))
    (when (fboundp 'test-flow--log)
      (test-flow--log "render: panel=%s sess=%s results=%s total=%s filters: status=%S name=%S tags=%S"
                      (or (bound-and-true-p test-flow--panel-buffer-name) (buffer-name))
                      (and sess (format "root=%s" (test-flow--session-root sess)))
                      (if (listp results) (number-to-string (length results)) "nil")
                      (or (and (listp sum) (alist-get 'total sum)) "?")
                      (and (boundp 'test-flow--panel-status-filter) test-flow--panel-status-filter)
                      (and (boundp 'test-flow--panel-name-regexp) test-flow--panel-name-regexp)
                      (and (boundp 'test-flow--panel-tags-filter) test-flow--panel-tags-filter))
      (let ((cnt (apply #'test-flow--summary-counters (list sum results))))
        (test-flow--log "render: counters P:%s F:%s E:%s U:%s"
                        (plist-get cnt :passed)
                        (plist-get cnt :failed)
                        (plist-get cnt :error)
                        (plist-get cnt :unexpected)))
      (when (null results)
        (test-flow--log "no-results: none stored yet (root=%s) proc-live=%s first-open-done=%s run-on-open=%s runner=%s cmd=%S active=%s queued=%s"
                        (and sess (test-flow--session-root sess))
                        (and proc (process-live-p proc))
                        (test-flow--conf sess 'first-open-run-done nil)
                        (and (boundp 'test-flow-run-on-open) test-flow-run-on-open)
                        (test-flow--conf sess 'runner (and (boundp 'test-flow-runner) test-flow-runner))
                        (test-flow--conf sess 'external-command (and (boundp 'test-flow-external-command) test-flow-external-command))
                        (and (boundp 'test-flow--active-run-count) test-flow--active-run-count)
                        (and (boundp 'test-flow--run-queue) (length test-flow--run-queue))))
      (when (and (listp results) (= (length results) 0))
        (test-flow--log "no-results: empty list (root=%s) total=%s last-parser=%s"
                        (and sess (test-flow--session-root sess))
                        (or (and (listp sum) (alist-get 'total sum)) "?")
                        (test-flow--conf sess 'parser (and (boundp 'test-flow-parser) test-flow-parser)))))
    (list :sess sess :sum sum :results results :proc proc)))

(defun test-flow-render-render-insert (ctx)
  "Insert either spinner or grouped suites using CTX (no Status/Coverage in main panel)."
  (let* ((sess (plist-get ctx :sess))
         (sum (plist-get ctx :sum))
         (results (plist-get ctx :results))
         (proc (plist-get ctx :proc)))
    ;; Show prominent parse error messages from stderr, if present
    (let* ((stderr (and (fboundp 'test-flow--get-last-stderr-output)
                        (ignore-errors (test-flow--get-last-stderr-output sess))))
           (msg
            (when (stringp stderr)
              (or
               ;; EOF parse error with explicit path
               (when (string-match "End of file during parsing:[ \t]+\\(.+\\)" stderr)
                 (let* ((full (string-trim (match-string 1 stderr)))
                        (nm (file-name-nondirectory (directory-file-name full))))
                   (format "End of file during parsing: %s" (or nm full))))
               ;; Generic read/parse errors: try to extract file path from backtrace
               (let* ((err-kind (cond
                                 ((string-match "Invalid read syntax" stderr) "Invalid read syntax")
                                 ((string-match "invalid-read-syntax" stderr) "Invalid read syntax")
                                 ((string-match "End of file during parsing" stderr) "End of file during parsing")
                                 (t nil)))
                      (file (or
                             (and (string-match "load-with-code-conversion(\"\\([^\"]+\\.el\\)\"" stderr)
                                  (match-string 1 stderr))
                             (and (string-match "load(\"\\([^\"]+\\.el\\)\"" stderr)
                                  (match-string 1 stderr))
                             ;; Fallback: any absolute .el path
                             (and (string-match "\\(/[^ \n\t\"]+\\.el\\)" stderr)
                                  (match-string 1 stderr)))))
                 (when (or err-kind file)
                   (let* ((full (string-trim (or file "")))
                          (nm (and (> (length full) 0)
                                   (file-name-nondirectory (directory-file-name full)))))
                     (format "%s: %s" (or err-kind "Read error")
                             (if (and nm (> (length nm) 0)) nm full)))))))))
      (when msg
        (insert (propertize (concat "  " msg "\n\n") 'face 'error))))
    ;; If a run is active for this session, show spinner + progress % instead of full suite list.
    (if (and proc (process-live-p proc))
        (let* ((frame (if (fboundp 'test-flow-spinner-frame) (test-flow-spinner-frame) "⠋"))
               (pct (if (fboundp 'test-flow-spinner-percent-from-session)
                        (test-flow-spinner-percent-from-session sess proc)
                      nil))
               (pct-str (when (numberp pct) (format "%d%%" (truncate (* 100 pct))))))
          ;; Prepare a single-line spinner region that can be updated in-place by timer
          (let ((line (format "  %s %s\n" frame (or pct-str "Running..."))))
            (put-text-property 0 (length line) 'test-flow--spinner-region t line)
            ;; Begin marker must not advance on insert; keep it anchored at line start.
            (setq-local test-flow--spinner-beg (copy-marker (point)))
            (insert line)
            ;; End marker should advance to stay after inserted spinner line.
            (setq-local test-flow--spinner-end (copy-marker (point) t)))
          (insert "\n")
          (insert (propertize "  Press $ to view live output\n\n" 'face 'shadow)))
      ;; Not running: clear spinner markers and show grouped suites
      (setq-local test-flow--spinner-beg nil)
      (setq-local test-flow--spinner-end nil)
      (dolist (pair (test-flow-render-group-results
                     (if (fboundp 'test-flow--apply-panel-filters)
                         (test-flow--apply-panel-filters results)
                       results)))
        (test-flow-render-insert-suite (car pair) (cdr pair))))))

(defun test-flow-render-render-restore-point ()
  "Restore point after rendering to suite header if requested, else to beginning."
  (if (and (boundp 'test-flow--restore-point-suite) test-flow--restore-point-suite)
      (progn
        (test-flow--goto-suite-heading test-flow--restore-point-suite)
        (setq test-flow--restore-point-suite nil))
    (goto-char (point-min))))

(defun test-flow-render-render ()
  "Render the panel for the session associated with `test-flow--panel-buffer-name'."
  (let ((buf (get-buffer-create (or (bound-and-true-p test-flow--panel-buffer-name)
                                    (buffer-name)))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (let* ((ctx (test-flow-render-render-context))
               (sess (plist-get ctx :sess))
               (sum (plist-get ctx :sum))
               (results (plist-get ctx :results)))
          (when sess
            (if (local-variable-p 'test-flow--panel-status-folded (current-buffer))
                (setq-local test-flow--panel-status-initialized t)
              (unless (and (boundp 'test-flow--panel-status-initialized)
                           test-flow--panel-status-initialized)
                (setq-local test-flow--panel-status-folded
                            (test-flow--conf sess 'panel-status-folded t))
                (setq-local test-flow--panel-status-initialized t))))

          (test-flow-render-render-insert ctx))
        (test-flow-render-render-restore-point)))))

;;; Compatibility shims (monolith-era render names)

(unless (fboundp 'test-flow--render)
  (defalias 'test-flow--render 'test-flow-render-render))

(unless (fboundp 'test-flow--render-context)
  (defalias 'test-flow--render-context 'test-flow-render-render-context))

(unless (fboundp 'test-flow--render-insert)
  (defalias 'test-flow--render-insert 'test-flow-render-render-insert))

(unless (fboundp 'test-flow--render-restore-point)
  (defalias 'test-flow--render-restore-point 'test-flow-render-render-restore-point))

(unless (fboundp 'test-flow--status-icon)
  (defalias 'test-flow--status-icon 'test-flow-render-status-icon))

(unless (fboundp 'test-flow--status-face)
  (defalias 'test-flow--status-face 'test-flow-render-status-face))

(unless (fboundp 'test-flow--status-line-icon)
  (defalias 'test-flow--status-line-icon 'test-flow-render-status-line-icon))

(defun test-flow-render--suite-button-action (btn)
  "Button action to toggle the suite group stored on BTN."
  (let ((suite (button-get btn 'test-flow--suite)))
    (when suite
      (when (fboundp 'test-flow--ensure-fold-table)
        (test-flow--ensure-fold-table))
      (let ((cur (and (boundp 'test-flow--folded-suites)
                      (gethash suite test-flow--folded-suites))))
        (puthash suite (not cur) test-flow--folded-suites))
      (let ((test-flow--panel-buffer-name (buffer-name)))
        (setq-local test-flow--restore-point-suite suite)
        (when (fboundp 'test-flow--render)
          (test-flow--render))))))

(provide 'test-flow-render)
;;; test-flow-render.el ends here
