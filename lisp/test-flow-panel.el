;;; test-flow-panel.el --- Panel UI for test-flow (minimal impl) -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Minimal panel implementation to decouple from the monolith.
;; Provides:
;; - test-flow-panel-mode (major-mode for panel buffer)
;; - basic status block insertion (test-flow--insert-status-block)
;; - filters and group fold helpers
;; - open/toggle panel
;; - lightweight dashboard (for tests)
;;
;; Rendering pipeline functions live in test-flow-render.el; we only provide
;; helpers/hooks used by the renderer and tests.

;;; Code:

(require 'cl-lib)
(eval-when-compile (require 'subr-x))
(require 'test-flow-core)
(require 'test-flow-render)

;;;###autoload
(defgroup test-flow-panel nil
  "Panel UI for test-flow."
  :group 'test-flow)

;; Core panel options (used by core init and render; dir-locals can override)
(defcustom test-flow-panel-side 'right
  "Panel side for displaying ERT results."
  :type '(choice (const right) (const bottom) (const left) (const top))
  :group 'test-flow-panel)

(defcustom test-flow-panel-width 42
  "Panel width in columns for side window display."
  :type 'integer
  :group 'test-flow-panel)

(defcustom test-flow-icons t
  "Whether to show unicode/status icons for tests."
  :type 'boolean
  :group 'test-flow-panel)

(defcustom test-flow-toolbar-style 'auto
  "Style for icons in toolbar/header-line: 'auto | 'icons | 'text."
  :type '(choice (const auto) (const icons) (const text))
  :group 'test-flow-panel)

(defcustom test-flow-auto-detect-on-open t
  "If non-nil, try to auto-detect external command when opening the panel.

Heuristics:
- tests/run-tests.el or test/run-tests.el under project root
- flake.nix (apps.tests → `nix run .#tests`, checks.* → `nix build` targets)
- fallback to `nix flake check` if nothing specific matched"
  :type 'boolean
  :group 'test-flow-panel)

(defcustom test-flow-run-on-open t
  "If non-nil, run tests once upon opening the panel when no results are present.

This first-run happens only once per session per Emacs session and respects the
selected runner:
- external-command: runs if a command is configured or auto-detected
- in-emacs-ert: runs unconditionally"
  :type 'boolean
  :group 'test-flow-panel)

(defcustom test-flow-run-on-enable nil
  "If non-nil, run tests once when `test-flow-mode' is enabled."
  :type 'boolean
  :group 'test-flow-panel)

;; Declarations and shared state
(defvar test-flow--active-run-count 0)
(defvar test-flow--run-queue nil)
(defvar test-flow--sessions nil) ;; comes from core
(declare-function test-flow--render "test-flow-render" ())
(declare-function test-flow-headerline--apply "test-flow-headerline" (buffer))
(declare-function test-flow-view-controls--ensure-headerline-face "test-flow-view-controls" ())

;; Panel-local state
(defvar-local test-flow--folded-suites nil
  "Hash table of fold state for suite groups in the current panel buffer.")

(defvar-local test-flow--panel-status-folded t
  "If non-nil, the status block is folded in this panel buffer.")
(defvar-local test-flow--panel-status-initialized nil
  "Internal: if non-nil, do not overwrite fold state from session conf during render.")

(defun test-flow--ensure-fold-table ()
  "Ensure the panel buffer has a fold table initialized."
  (unless (and (boundp 'test-flow--folded-suites)
               (hash-table-p test-flow--folded-suites))
    (setq-local test-flow--folded-suites (make-hash-table :test 'equal))))

;;;###autoload
(define-derived-mode test-flow-panel-mode special-mode "test-flow_panel"
  "Major mode for displaying ERT results in a side panel."
  (setq buffer-read-only t
        truncate-lines t)
  ;; Ensure invisible prefixes (e.g., "<suite>/") are hidden visually in this buffer.
  (let ((bis buffer-invisibility-spec))
    (setq-local buffer-invisibility-spec
                (cond
                 ((eq bis t) t)
                 ((listp bis)
                  (if (memq 'test-flow-hide bis) bis (cons 'test-flow-hide bis)))
                 (t (list 'test-flow-hide)))))
  ;; Buffer-local fold state for suite groups
  (setq-local test-flow--folded-suites (or test-flow--folded-suites
                                           (make-hash-table :test 'equal)))
  ;; Header-line integration (optional)
  (ignore-errors (require 'test-flow-headerline nil t))
  (when (and (boundp 'test-flow-view-headerline-enable)
             test-flow-view-headerline-enable
             (fboundp 'test-flow-headerline--apply))
    (test-flow-headerline--apply (current-buffer)))
  (when (fboundp 'test-flow-view-controls--ensure-headerline-face)
    (ignore-errors (test-flow-view-controls--ensure-headerline-face))))

;; Helpers required by renderer and controls

(defun test-flow--find-panel-session ()
  "Return session object for the current panel buffer."
  (let* ((panel-name
          (or (and (eq major-mode 'test-flow-panel-mode) (buffer-name))
              (and (stringp test-flow--panel-buffer-name) test-flow--panel-buffer-name)
              (buffer-name)))
         (found nil))
    (maphash
     (lambda (_root s)
       (when (and (not found)
                  (string= (test-flow--session-panel-buf-name s) panel-name))
         (setq found s)))
     test-flow--sessions)
    (or found (test-flow--get-session (test-flow--project-root)))))

(defun test-flow--apply-panel-filters (results)
  "Apply panel-local filters to RESULTS and return a filtered list."
  (let ((rs (or results '())))
    (if (not (eq major-mode 'test-flow-panel-mode))
        rs
      (when (and (boundp 'test-flow--panel-status-filter)
                 test-flow--panel-status-filter)
        (setq rs (seq-filter
                  (lambda (r) (memq (plist-get r :status) test-flow--panel-status-filter))
                  rs)))
      (when (and (boundp 'test-flow--panel-name-regexp)
                 (stringp test-flow--panel-name-regexp)
                 (> (length test-flow--panel-name-regexp) 0))
        (setq rs (seq-filter
                  (lambda (r)
                    (let ((nm (or (plist-get r :name) "")))
                      (ignore-errors (string-match-p test-flow--panel-name-regexp nm))))
                  rs)))
      (when (and (boundp 'test-flow--panel-tags-filter)
                 (listp test-flow--panel-tags-filter)
                 test-flow--panel-tags-filter)
        (setq rs (seq-filter
                  (lambda (r)
                    (let ((tags (plist-get r :tags)))
                      (and (listp tags)
                           (seq-some (lambda (want)
                                       (seq-some (lambda (have) (equal (format "%s" have) want)) tags))
                                     test-flow--panel-tags-filter))))
                  rs)))
      rs)))

(defun test-flow--summary-counters (sum results)
  "Compute counters and duration fields from SUM/RESULTS."
  (let* ((total (or (alist-get 'total sum) 0))
         (unexpected (or (alist-get 'unexpected sum) 0))
         (p (or (alist-get 'passed sum)
                (cl-count-if (lambda (r) (eq (plist-get r :status) 'pass)) results)))
         (f (or (alist-get 'failed sum)
                (cl-count-if (lambda (r) (eq (plist-get r :status) 'fail)) results)))
         (e (or (alist-get 'error sum)
                (cl-count-if (lambda (r) (eq (plist-get r :status) 'error)) results)))
         (s (or (alist-get 'skipped sum)
                (cl-count-if (lambda (r) (memq (plist-get r :status) '(skip xfail))) results)))
         (time-str (alist-get 'time sum))
         (dur-ms (alist-get 'duration-ms sum))
         (dur-str (cond
                   ((numberp dur-ms) (format "%.3fs" (/ dur-ms 1000.0)))
                   (time-str (format "%ss" time-str))
                   (t "-"))))
    (list :total total :unexpected unexpected :passed p :failed f :error e :skipped s
          :dur-str dur-str)))

(defun test-flow--status-line-icon (key &optional state)
  "Delegate to render module for status-line icons."
  (if (fboundp 'test-flow-render-status-line-icon)
      (test-flow-render-status-line-icon key state)
    ""))

(defun test-flow--insert-status-block (sess sum results)
  "Insert a compact Status block with meta info and counters."
  (let* ((folded test-flow--panel-status-folded)
         (arrow (if folded "▸" "▾"))
         (icon (test-flow--status-line-icon 'counters))
         (label-base (concat arrow " " icon " Status"))
         (cnt (apply #'test-flow--summary-counters (list sum results)))
         (dur-str (plist-get cnt :dur-str))
         (total (plist-get cnt :total))
         (passed (plist-get cnt :passed))
         (failed (plist-get cnt :failed))
         (error  (plist-get cnt :error))
         (skipped (plist-get cnt :skipped))
         (unexpected (plist-get cnt :unexpected))
         (runner-sym (test-flow--conf sess 'runner (if (boundp 'test-flow-runner) test-flow-runner 'external-command)))
         (runner (if (eq runner-sym 'in-emacs-ert) "in-emacs" "external"))
         (parser-used (or (test-flow--get-last-parser sess)
                          (test-flow--conf sess 'parser (if (boundp 'test-flow-parser) test-flow-parser 'auto))))
         (parser-str (format "%s" parser-used))
         (active (and (boundp 'test-flow--active-run-count) test-flow--active-run-count))
         (queued (and (boundp 'test-flow--run-queue) (length test-flow--run-queue)))
         (wmode (test-flow--conf sess 'watch-mode (if (boundp 'test-flow-watch-mode) test-flow-watch-mode 'after-save)))
         (watch-on (test-flow--get-watch-enabled sess)))
    (let ((beg (point)))
      (insert-text-button
       (concat label-base
               (when folded
                 (format " %d (P:%d F:%d E:%d S:%d U:%d)"
                         (or total 0) (or passed 0) (or failed 0)
                         (or error 0) (or skipped 0) (or unexpected 0)))
               "\n")
       'face '(:inherit default :underline nil)
       'mouse-face nil
       'follow-link t
       'help-echo "Fold/unfold Status (mouse-1)"
       'action (lambda (_btn) (test-flow-toggle-status)))
      ;; Re-apply per-metric faces over the button face in folded state
      (when folded
        (let* ((end (point))
               (base-len (length label-base))
               (cstart (+ beg base-len)))
          (when (< cstart end)
            (save-excursion
              (cl-labels
                  ((apply-metric (key pos-face)
                     (goto-char cstart)
                     (while (re-search-forward (format "%s:\\([0-9]+\\)" key) end t)
                       (let* ((n (string-to-number (match-string 1)))
                              (face (if (> n 0) pos-face 'shadow)))
                         (add-text-properties (match-beginning 0) (match-end 0)
                                              (list 'face face))))))
                (apply-metric "P" 'test-flow-face-pass)
                (apply-metric "F" 'test-flow-face-fail)
                (apply-metric "E" 'test-flow-face-error)
                (apply-metric "S" 'test-flow-face-skip)
                ;; U>0 treated as failure color; U=0 shown in gray
                (apply-metric "U" 'test-flow-face-fail)))))))
    (unless folded
      (cl-labels ((mk (icon body)
                    (insert (format "  %s %s\n" icon body))))
        (mk (test-flow--status-line-icon 'counters)
            (test-flow-render-format-counters sum results))
        (mk (test-flow--status-line-icon 'duration)
            (format "duration: %s" (or dur-str "-")))
        (mk (test-flow--status-line-icon 'proc)
            (format "Proc: active %s, queued %s" (or active 0) (or queued 0)))
        (mk (test-flow--status-line-icon 'runner)
            (format "Runner: %s" runner))
        (mk (test-flow--status-line-icon 'mode)
            (format "Mode: %s" wmode))
        (mk (test-flow--status-line-icon 'watch (if watch-on 'on 'off))
            (format "Watch: %s" (if watch-on "On" "Off")))
        (mk (test-flow--status-line-icon 'parser)
            (format "Parser: %s" parser-str))))
    (insert "\n")))

;;;###autoload
(defun test-flow-toggle-status ()
  "Toggle folding of the Status block and re-render."
  (interactive)
  (setq test-flow--panel-status-folded (not test-flow--panel-status-folded))
  (let ((sess (ignore-errors (test-flow--find-panel-session))))
    (when sess
      (test-flow--set-conf sess 'panel-status-folded test-flow--panel-status-folded)))
  (let ((test-flow--panel-buffer-name (buffer-name)))
    (test-flow--render)))

(defun test-flow--goto-suite-heading (suite)
  "Move point to the heading line of SUITE. Return non-nil on success."
  (goto-char (point-min))
  (catch 'found
    (while (not (eobp))
      (when (equal (get-text-property (line-beginning-position) 'test-flow--suite) suite)
        (beginning-of-line)
        (throw 'found t))
      (forward-line 1))
    nil))

;;;###autoload
(defun test-flow-toggle-group-at-point ()
  "Toggle folding of suite group at point (on header) or the group containing the current test line."
  (interactive)
  (test-flow--ensure-fold-table)
  (let* ((suite-here (get-text-property (line-beginning-position) 'test-flow--suite))
         (test-here (get-text-property (line-beginning-position) 'test-flow--result))
         (suite (cond
                 (suite-here suite-here)
                 (test-here
                  (save-excursion
                    (beginning-of-line)
                    (catch 'found
                      (while (not (bobp))
                        (forward-line -1)
                        (let ((s (get-text-property (line-beginning-position) 'test-flow--suite)))
                          (when s (throw 'found s))))
                      nil)))
                 (t nil))))
    (when suite
      (let ((cur (gethash suite test-flow--folded-suites)))
        (puthash suite (not cur) test-flow--folded-suites))
      (let ((test-flow--panel-buffer-name (buffer-name)))
        (setq-local test-flow--restore-point-suite suite)
        (test-flow--render)))))

;;;###autoload
(defun test-flow-toggle-all-groups (&optional expand)
  "Toggle folding for all groups. With EXPAND non-nil, unfold all."
  (interactive "P")
  (test-flow--ensure-fold-table)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (let ((suite (get-text-property (point) 'test-flow--suite)))
        (when suite
          (puthash suite (not expand) test-flow--folded-suites)))
      (forward-line 1)))
  (let ((test-flow--panel-buffer-name (buffer-name)))
    (test-flow--render)))

;;;###autoload
(defun test-flow-open-panel ()
  "Open or focus the test-flow panel for the current project."
  (interactive)
  (let* ((root (test-flow--project-root))
         (sess (test-flow--get-session root))
         ;; Try lightweight auto-detect of external command on first open
         (runner (test-flow--conf sess 'runner (if (boundp 'test-flow-runner) test-flow-runner 'external-command)))
         (ext    (test-flow--conf sess 'external-command (and (boundp 'test-flow-external-command) test-flow-external-command)))
         (side (test-flow--conf sess 'panel-side (if (boundp 'test-flow-panel-side) test-flow-panel-side 'right)))
         (width (test-flow--conf sess 'panel-width (if (boundp 'test-flow-panel-width) test-flow-panel-width 42)))
         (bufname (test-flow--session-panel-name root))
         (buf (get-buffer-create bufname)))
    (when (and (boundp 'test-flow-auto-detect-on-open) test-flow-auto-detect-on-open
               (eq runner 'external-command)
               (null ext))
      (let* ((cand1 (expand-file-name "tests/run-tests.el" root))
             (cand2 (expand-file-name "t/run-tests.el" root)))
        (cond
         ((file-exists-p cand1)
          (test-flow--set-conf sess 'external-command (list "emacs" "-Q" "--batch" "-l" cand1))
          (test-flow--set-conf sess 'runner 'external-command))
         ((file-exists-p cand2)
          (test-flow--set-conf sess 'external-command (list "emacs" "-Q" "--batch" "-l" cand2))
          (test-flow--set-conf sess 'runner 'external-command)))))
    (with-current-buffer buf
      (setq default-directory (file-name-as-directory root))
      (test-flow-panel-mode))
    (let ((win (display-buffer-in-side-window buf `((side . ,side) (window-width . ,width)))))
      (when (and win (called-interactively-p 'interactive))
        (select-window win)))
    (when (and (boundp 'test-flow-status-open-on-panel-open)
               test-flow-status-open-on-panel-open
               (fboundp 'test-flow-status-open))
      (with-current-buffer buf
        (ignore-errors (test-flow-status-open))))
    (let ((test-flow--panel-buffer-name bufname))
      (test-flow--render))))

;;;###autoload
(defun test-flow-toggle-panel ()
  "Toggle the test-flow panel for the current project."
  (interactive)
  (let* ((root (test-flow--project-root))
         (bufname (test-flow--session-panel-name root))
         (buf (get-buffer bufname))
         (wins (when buf (get-buffer-window-list buf nil t)))
         (closed nil))
    (when (and wins (> (length wins) 0))
      (dolist (w wins)
        (when (window-parameter w 'window-side)
          (delete-window w)
          (setq closed t))))
    (if closed
        (message "test-flow: panel closed")
      (test-flow-open-panel))))

;;;###autoload
(defun test-flow-dashboard ()
  "Show a minimal dashboard across test-flow sessions (for tests)."
  (interactive)
  (let ((buf (get-buffer-create "*test-flow: dashboard*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (special-mode)
        (insert (propertize "test-flow dashboard\n\n" 'face 'bold))))
    (display-buffer buf)))

;;;###autoload
(defun test-flow-copy-failures ()
  "Copy failures/errors from the last run into the kill-ring (delegates to test-flow-copy)."
  (interactive)
  (require 'test-flow-copy)
  (if (fboundp 'test-flow-copy--impl-copy-failures)
      (test-flow-copy--impl-copy-failures)
    (user-error "test-flow: copy module not available")))

;;;###autoload
(defun test-flow-toggle-logging ()
  "Toggle test-flow logging and report the new state."
  (interactive)
  (setq test-flow-log-enabled (not test-flow-log-enabled))
  (message "test-flow: logging %s" (if test-flow-log-enabled "enabled" "disabled")))

;; Simple panel filters API (used in tests)

(defun test-flow-panel-filter--set-status (statuses)
  "Helper: set STATUS filter to STATUSES (list or nil) and re-render."
  (setq-local test-flow--panel-status-filter statuses)
  (test-flow--render))

;;;###autoload (autoload 'test-flow-panel-filter-pass "test-flow-panel" nil t)
(defun test-flow-panel-filter-pass ()  (interactive) (test-flow-panel-filter--set-status '(pass)))
;;;###autoload (autoload 'test-flow-panel-filter-fail "test-flow-panel" nil t)
(defun test-flow-panel-filter-fail ()  (interactive) (test-flow-panel-filter--set-status '(fail)))
;;;###autoload (autoload 'test-flow-panel-filter-error "test-flow-panel" nil t)
(defun test-flow-panel-filter-error () (interactive) (test-flow-panel-filter--set-status '(error)))
;;;###autoload (autoload 'test-flow-panel-filter-skip "test-flow-panel" nil t)
(defun test-flow-panel-filter-skip ()  (interactive) (test-flow-panel-filter--set-status '(skip xfail)))
;;;###autoload (autoload 'test-flow-panel-filter-all "test-flow-panel" nil t)
(defun test-flow-panel-filter-all ()   (interactive) (test-flow-panel-filter--set-status nil))

;;;###autoload (autoload 'test-flow-panel-set-name-filter "test-flow-panel" nil t)
(defun test-flow-panel-set-name-filter (re)
  "Prompt for name regexp RE and re-render; empty input clears the filter."
  (interactive (list (read-string "Filter name (regexp, empty=clear): " (and (boundp 'test-flow--panel-name-regexp) test-flow--panel-name-regexp))))
  (setq-local test-flow--panel-name-regexp (if (string-empty-p re) nil re))
  (test-flow--render))

;;;###autoload (autoload 'test-flow-panel-set-tags-filter "test-flow-panel" nil t)
(defun test-flow-panel-set-tags-filter (tags)
  "Prompt for TAGS (comma-separated) and re-render; empty input clears the filter."
  (interactive (list (read-string "Filter tags (comma-separated, empty=clear): "
                                  (when (and (boundp 'test-flow--panel-tags-filter)
                                             test-flow--panel-tags-filter)
                                    (mapconcat #'identity test-flow--panel-tags-filter ",")))))
  (let ((trim (string-trim tags)))
    (setq-local test-flow--panel-tags-filter
                (if (string-empty-p trim)
                    nil
                  (seq-filter (lambda (s) (not (string-empty-p s)))
                              (mapcar #'string-trim (split-string trim ","))))))
  (test-flow--render))

;;;###autoload
(defun test-flow-panel-filter-clear ()
  "Clear all panel filters and re-render."
  (interactive)
  (setq-local test-flow--panel-status-filter nil)
  (setq-local test-flow--panel-name-regexp nil)
  (setq-local test-flow--panel-tags-filter nil)
  (test-flow--render))

;; Force modular render pipeline overrides (after any legacy copies).
;; This guarantees that panel uses test-flow-render.el implementations.
(ignore-errors (require 'test-flow-render))
(when (featurep 'test-flow-render)
  ;; Core pipeline
  (defalias 'test-flow--render               'test-flow-render-render)
  (defalias 'test-flow--render-context       'test-flow-render-render-context)
  (defalias 'test-flow--render-insert        'test-flow-render-render-insert)
  (defalias 'test-flow--render-restore-point 'test-flow-render-render-restore-point)
  ;; Pure helpers
  (defalias 'test-flow--status-icon          'test-flow-render-status-icon)
  (defalias 'test-flow--status-face          'test-flow-render-status-face)
  (defalias 'test-flow--status-line-icon     'test-flow-render-status-line-icon)
  (defalias 'test-flow--group-results        'test-flow-render-group-results)
  (defalias 'test-flow--insert-test-line     'test-flow-render-insert-test-line)
  (defalias 'test-flow--suite-aggregate      'test-flow-render-suite-aggregate)
  (defalias 'test-flow--suite-icon           'test-flow-render-suite-icon)
  (defalias 'test-flow--insert-suite         'test-flow-render-insert-suite))

;; ---------------------------------------------------------------------------
;; Panel keymaps and actions (buffer-local UX)
;; ---------------------------------------------------------------------------

;; Button map for suite headers: TAB folds group; n/p/j/k navigate
(defvar test-flow--suite-button-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map button-map)
    (define-key map (kbd "TAB") #'test-flow-toggle-group-at-point)
    (define-key map [tab] #'test-flow-toggle-group-at-point)
    (define-key map (kbd "<tab>") #'test-flow-toggle-group-at-point)
    (define-key map (kbd "<backtab>") #'test-flow-toggle-all-groups)
    (define-key map (kbd "n") #'test-flow-next-item)
    (define-key map (kbd "p") #'test-flow-previous-item)
    (define-key map (kbd "j") #'test-flow-next-item)
    (define-key map (kbd "k") #'test-flow-previous-item)
    map)
  "Keymap for suite heading buttons.")

;; Button map for Status header: TAB folds Status; n/p/j/k navigate
(defvar test-flow--status-button-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map button-map)
    (define-key map (kbd "TAB") #'test-flow-toggle-status)
    (define-key map [tab] #'test-flow-toggle-status)
    (define-key map (kbd "<tab>") #'test-flow-toggle-status)
    (define-key map (kbd "n") #'test-flow-next-item)
    (define-key map (kbd "p") #'test-flow-previous-item)
    (define-key map (kbd "j") #'test-flow-next-item)
    (define-key map (kbd "k") #'test-flow-previous-item)
    map)
  "Keymap for Status header button.")

;; Ensure panel buffer has convenient hotkeys (add to existing mode map)
(when (boundp 'test-flow-panel-mode-map)
  (let ((map test-flow-panel-mode-map))
    ;; Run/Watch/Copy/Clear/Detect
    (define-key map (kbd "g") #'test-flow-run)
    (define-key map (kbd "r") #'test-flow-run)
    (define-key map (kbd "f") #'test-flow-run-failed)
    (define-key map (kbd "w") #'test-flow-toggle-watch)
    (define-key map (kbd "y") #'test-flow-copy-failures)
    (define-key map (kbd "x") #'test-flow-clear)
    (define-key map (kbd "d") #'test-flow-detect-runner)
    ;; Live progress
    (define-key map (kbd "$") #'test-flow-show-progress)
    ;; Navigation & details
    (define-key map (kbd "n") #'test-flow-next-item)
    (define-key map (kbd "p") #'test-flow-previous-item)
    (define-key map (kbd "j") #'test-flow-next-item)
    (define-key map (kbd "k") #'test-flow-previous-item)
    (define-key map (kbd "RET") #'test-flow-open-details-at-point)
    (define-key map (kbd "o") #'test-flow-goto-definition-at-point)
    ;; Folding
    (define-key map (kbd "TAB") #'test-flow-tab)
    (define-key map (kbd "<backtab>") #'test-flow-toggle-all-groups)
    ;; Filters (status/name/tags)
    (define-key map (kbd "P") #'test-flow-panel-filter-pass)
    (define-key map (kbd "F") #'test-flow-panel-filter-fail)
    (define-key map (kbd "E") #'test-flow-panel-filter-error)
    (define-key map (kbd "s") #'test-flow-panel-filter-skip)
    (define-key map (kbd "A") #'test-flow-panel-filter-all)
    (define-key map (kbd "/") #'test-flow-panel-set-name-filter)
    (define-key map (kbd "T") #'test-flow-panel-set-tags-filter)
    (define-key map (kbd "C") #'test-flow-panel-filter-clear)
    ;; Status split toggle
    (define-key map (kbd "S") #'test-flow-status-toggle)))

;; Minimal details view
(defun test-flow--details-populate-simple (buf name details)
  (with-current-buffer buf
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (propertize (format "%s\n\n" name) 'face 'mode-line-buffer-id))
      (insert (or details ""))
      (goto-char (point-min))
      (view-mode 1))))

;;;###autoload
(defun test-flow-open-details-at-point ()
  "Open a buffer with details for the test at point (session-aware)."
  (interactive)
  (let* ((r (get-text-property (line-beginning-position) 'test-flow--result)))
    (unless r (user-error "No test result at point"))
    (let* ((root (test-flow--project-root))
           (bufname (test-flow--session-details-name root))
           (buf (get-buffer-create bufname))
           (name (plist-get r :name))
           (details (or (plist-get r :details) "No details")))
      (test-flow--details-populate-simple buf name details)
      (display-buffer buf))))

;;;###autoload
(defun test-flow-goto-definition-at-point ()
  "Jump to the ERT test definition at point if available."
  (interactive)
  (let* ((r (get-text-property (line-beginning-position) 'test-flow--result))
         (name (and r (plist-get r :name))))
    (unless name (user-error "No test result at point"))
    (let ((sym (intern-soft name)))
      (unless (and sym (fboundp sym))
        (user-error "Test %s is not loaded; load it in this Emacs to jump" name))
      (find-function sym))))

;; Navigation helpers
(defun test-flow--goto-next-item (dir)
  "Move to next (DIR>0) or previous (DIR<0) navigable item."
  (let ((step (if (> dir 0) 1 -1))
        (start (point))
        (done nil))
    (catch 'done
      (while (if (> dir 0) (not (eobp)) (not (bobp)))
        (forward-line step)
        (let ((is-test (get-text-property (line-beginning-position) 'test-flow--result))
              (is-suite (get-text-property (line-beginning-position) 'test-flow--suite))
              (nav     (get-text-property (line-beginning-position) 'test-flow--nav)))
          (when (or nav is-test is-suite)
            (beginning-of-line)
            (setq done t)
            (throw 'done t)))))
    (unless done
      (goto-char start)
      (message "test-flow: no more items"))))

;;;###autoload
(defun test-flow-next-item () (interactive) (test-flow--goto-next-item 1))
;;;###autoload
(defun test-flow-previous-item () (interactive) (test-flow--goto-next-item -1))

;;;###autoload
(defun test-flow-tab ()
  "Context-aware TAB: toggle Status at Status header or toggle suite group."
  (interactive)
  (let* ((nav (get-text-property (line-beginning-position) 'test-flow--nav))
         (suite (get-text-property (line-beginning-position) 'test-flow--suite)))
    (cond
     ((or (eq nav 'status) (eq nav 'status-item))
      (test-flow-toggle-status))
     (suite (test-flow-toggle-group-at-point))
     (t (test-flow-toggle-group-at-point)))))

;;;###autoload
(defun test-flow-clear ()
  "Clear panel and last results for the current session; re-render."
  (interactive)
  (let* ((sess (test-flow--get-session (test-flow--project-root))))
    (when sess
      (test-flow--set-last-summary sess nil)
      (test-flow--set-last-results sess nil)
      (test-flow--set-last-raw-output sess nil)))
  (let* ((bufname (test-flow--session-panel-name (test-flow--project-root))))
    (when (get-buffer bufname)
      (let ((test-flow--panel-buffer-name bufname))
        (test-flow--render))))
  (message "test-flow: cleared"))

;;;###autoload
(define-minor-mode test-flow-mode
  "Global minor mode for Test Flow.

When enabled, opens the panel. If `test-flow-run-on-enable' is non-nil,
runs tests immediately using `test-flow-run'.

When disabled, closes side-window панели текущего проекта (буферы панелей не
уничтожаются)."
  :global t
  :group 'test-flow-panel
  (if test-flow-mode
      (progn
        (test-flow-open-panel)
        (when (and (boundp 'test-flow-run-on-enable) test-flow-run-on-enable
                   (fboundp 'test-flow-run))
          (test-flow-run)))
    ;; on disable: close panel window but keep buffers
    (let* ((root (test-flow--project-root))
           (buf (get-buffer (test-flow--session-panel-name root))))
      (when buf
        (dolist (w (get-buffer-window-list buf nil t))
          (when (window-parameter w 'window-side)
            (delete-window w)))))))

;;;; Status split window

(defcustom test-flow-status-open-on-panel-open t
  "If non-nil, automatically open the Status split when opening the panel."
  :type 'boolean
  :group 'test-flow-panel)

(defcustom test-flow-status-split-side 'same
  "Side where the Status split is displayed.
Use 'same to match the panel side."
  :type '(choice (const same) (const bottom) (const top) (const left) (const right))
  :group 'test-flow-panel)

(defcustom test-flow-status-split-height 0.25
  "Height (fraction) of the Status split window."
  :type 'number
  :group 'test-flow-panel)

(defun test-flow--status-buffer-name (root)
  "Return status buffer name for project ROOT."
  (format "*test-flow: status %s*" (file-name-nondirectory (directory-file-name root))))

(defvar test-flow-status-mode-map
  (let ((map (make-sparse-keymap())))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "q") #'test-flow-status-close)
    (define-key map (kbd "S") #'test-flow-status-close)
    map)
  "Keymap for `test-flow-status-mode' (q/S closes the split).")

(define-derived-mode test-flow-status-mode special-mode "test-flow_status"
  "Major mode for the Test Flow status split buffer."
  (setq buffer-read-only t
        truncate-lines t))

(defun test-flow-status-visible-p (&optional root)
  "Return non-nil if the Status split is visible for ROOT (current project if nil)."
  (let* ((r (or root (test-flow--project-root)))
         (bufname (test-flow--status-buffer-name r))
         (buf (get-buffer bufname)))
    (and buf (get-buffer-window buf 'visible))))

;;;###autoload
(defun test-flow-view-controls--status-state ()
  "Return 'open or 'closed based on visibility of the Status split for current project."
  (if (test-flow-status-visible-p) 'open 'closed))

(defun test-flow-status--render-into (buf sess)
  "Render status into BUF for SESS (expanded)."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (test-flow-status-mode)
        ;; Force expanded status in this view
        (setq-local test-flow--panel-status-folded nil)
        (let ((sum (and sess (test-flow--get-last-summary sess)))
              (res (and sess (test-flow--get-last-results sess))))
          (test-flow--insert-status-block sess sum res))
        (goto-char (point-min))))))

;;;###autoload
(defun test-flow-status-open ()
  "Open Status split for current panel/session."
  (interactive)
  (let* ((sess (or (and (eq major-mode 'test-flow-panel-mode)
                        (ignore-errors (test-flow--find-panel-session)))
                   (test-flow--get-session (test-flow--project-root))))
         (root (and sess (test-flow--session-root sess)))
         (bufname (test-flow--status-buffer-name (or root (test-flow--project-root))))
         (buf (get-buffer-create bufname)))
    (test-flow-status--render-into buf sess)
    (let* ((panel-side (or (and sess (test-flow--conf sess 'panel-side (if (boundp 'test-flow-panel-side) test-flow-panel-side 'right)))
                           (if (boundp 'test-flow-panel-side) test-flow-panel-side 'right)))
           (side (if (eq test-flow-status-split-side 'same) panel-side test-flow-status-split-side))
           ;; Для right/left используем window-width (в колонках),
           ;; для top/bottom — window-height (доля).
           (panel-w (or (and sess (test-flow--conf sess 'panel-width (if (boundp 'test-flow-panel-width) test-flow-panel-width 42)))
                        42))
           (params `((side . ,side)
                     ;; Выделяем отдельный слот, чтобы не заменять окно панели.
                     (slot . 1))))
      (setq params
            (if (memq side '(left right))
                (append params `((window-width . ,panel-w)))
              (append params `((window-height . ,test-flow-status-split-height)))))
      (display-buffer-in-side-window buf params))
    (when (fboundp 'test-flow-headerline-refresh)
      (test-flow-headerline-refresh))
    (force-mode-line-update t)))

;;;###autoload
(defun test-flow-status-close ()
  "Close the Status split window (and kill its buffer)."
  (interactive)
  (let* ((root (test-flow--project-root))
         (bufname (test-flow--status-buffer-name root))
         (buf (get-buffer bufname)))
    (when buf
      (dolist (w (get-buffer-window-list buf nil t))
        (when (window-live-p w)
          (delete-window w)))
      (kill-buffer buf)))
  (when (fboundp 'test-flow-headerline-refresh)
    (test-flow-headerline-refresh))
  (force-mode-line-update t))

;;;###autoload
(defun test-flow-status-toggle ()
  "Toggle the Status split for the current session."
  (interactive)
  (if (test-flow-status-visible-p)
      (test-flow-status-close)
    (test-flow-status-open)))

;; Refresh status split after each run if visible
(defun test-flow-status--after-run-refresh (sess _summary _results)
  (when (and sess
             (let ((root (test-flow--session-root sess)))
               (test-flow-status-visible-p root)))
    (let* ((root (test-flow--session-root sess))
           (bufname (test-flow--status-buffer-name root))
           (buf (get-buffer-create bufname)))
      (test-flow-status--render-into buf sess))))

(add-hook 'test-flow-after-run-hook #'test-flow-status--after-run-refresh)

(provide 'test-flow-panel)
;;; test-flow-panel.el ends here
