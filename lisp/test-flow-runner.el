;;; test-flow-runner.el --- Runner facade and hooks for test-flow  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Provides runner-related commands and hooks.
;; For now, delegates to monolithic implementations from test-flow.el.

;;; Code:

(eval-when-compile (require 'subr-x))
(require 'seq)

;; Declarations used by modular implementations (from core/panel/runner/parse)
(declare-function test-flow--project-root "test-flow-core" ())
(declare-function test-flow--get-session "test-flow-core" (&optional root))
(declare-function test-flow--conf "test-flow-core" (sess key default))
(declare-function test-flow--normalize-command "test-flow-core" (value))
(declare-function test-flow--maybe-start-thunk "test-flow-runner" (sess label thunk))
;; Optional legacy in-Emacs runner; guarded by fboundp
;; (declare-function test-flow--start-in-emacs "test-flow-old" (sess selector label))
(declare-function test-flow--maybe-start-run "test-flow-runner" (sess cmd label))
;; Optional helpers; guarded by fboundp in code
;; (declare-function test-flow--failed-names "test-flow-panel" (results))
;; (declare-function test-flow--build-external-failed-cmd "test-flow-panel" (sess fails))
(declare-function test-flow--log "test-flow-core" (fmt &rest args))

;;;###autoload
(defgroup test-flow-runner nil
  "Runner configuration for test-flow."
  :group 'test-flow)

;;; Hooks (reserved for integrations; monolith can adopt later)
(defvar test-flow-before-run-hook nil
  "Hook run before starting a test run. Args: (sess).")

(defvar test-flow-after-run-hook nil
  "Hook run after a test run finishes. Args: (sess summary results).")

(defun test-flow-runner--require-monolith ()
  (require 'test-flow nil 'noerror))

;; Helpers to delegate safely without clobbering existing defs.

(defmacro test-flow-runner--define-wrapper (name &optional interactivep)
  "Define wrapper NAME delegating to monolith, unless already defined.
If INTERACTIVEP is non-nil, make it an interactive command."
  `(unless (fboundp ',name)
     (defun ,name ()
       ,(format "Wrapper delegating to monolithic `%s'." name)
       ,(when interactivep '(interactive))
       (let ((self (symbol-function ',name)))
         (test-flow-runner--require-monolith)
         (let ((impl (and (fboundp ',name) (symbol-function ',name))))
           (if (and impl (not (eq impl self)))
               (call-interactively ',name)
             (user-error "test-flow-runner: %s not available" ',name)))))))

;;;###autoload
(test-flow-runner--define-wrapper test-flow-run t)

;;;###autoload
(test-flow-runner--define-wrapper test-flow-run-failed t)

;;;###autoload
(test-flow-runner--define-wrapper test-flow-detect-runner t)

;;;###autoload
(test-flow-runner--define-wrapper test-flow-restart t)

;;;###autoload
(test-flow-runner--define-wrapper test-flow-dump-concurrency t)

;; -----------------------------------------------------------------------------
;; Modular implementations (delegate to monolith internals, stable API surface)
;; -----------------------------------------------------------------------------

;; We intentionally do not (require 'test-flow) at top to avoid load cycles:
;; - test-flow.el requires this module early via test-flow-modular
;; - these impls call monolithic internals at call time (they are loaded by then)

(defun test-flow-runner--impl-run ()
  "Run tests according to per-session runner (session-aware)."
  (interactive)
  (let* ((root (if (fboundp 'test-flow--project-root)
                   (test-flow--project-root)
                 default-directory))
         (sess (and (fboundp 'test-flow--get-session) (test-flow--get-session root)))
         (runner (if (and (fboundp 'test-flow--conf) (boundp 'test-flow-runner))
                     (test-flow--conf sess 'runner test-flow-runner)
                   'external-command)))
    (pcase runner
      ('in-emacs-ert
       (when (fboundp 'test-flow--log)
         (test-flow--log "run: in-emacs (root=%s)" root))
       (let ((selector t) (label "all(in-emacs)"))
         (if (fboundp 'test-flow--maybe-start-thunk)
             (test-flow--maybe-start-thunk
              sess label
              (lambda ()
                (when (fboundp 'test-flow--start-in-emacs)
                  (test-flow--start-in-emacs sess selector label))))
           (user-error "test-flow: in-emacs runner not available"))))
      (_
       (let* ((ext (if (fboundp 'test-flow--conf)
                       (test-flow--conf sess 'external-command
                                        (when (boundp 'test-flow-external-command)
                                          test-flow-external-command))
                     nil))
              (cmd (if (fboundp 'test-flow--normalize-command)
                       (test-flow--normalize-command ext)
                     ext)))
         (unless cmd
           (when (fboundp 'test-flow--log)
             (test-flow--log "run: external-command missing (root=%s ext=%S)" root ext))
           (user-error "Set per-session external command (M-x test-flow-detect-runner)"))
         (when (fboundp 'test-flow--log)
           (test-flow--log "run: external (root=%s cmd=%S)" root cmd))
         (if (fboundp 'test-flow--maybe-start-run)
             (test-flow--maybe-start-run sess cmd "all")
           (user-error "test-flow: external runner not available")))))))

(defun test-flow-runner--impl-run-failed ()
  "Run only failed/error tests if possible, else run all (session-aware)."
  (interactive)
  (let* ((root (if (fboundp 'test-flow--project-root)
                   (test-flow--project-root)
                 default-directory))
         (sess (and (fboundp 'test-flow--get-session) (test-flow--get-session root)))
         (results (or (and sess (fboundp 'test-flow--session-last-results)
                           (test-flow--session-last-results sess))
                      (and (boundp 'test-flow--last-results) test-flow--last-results)))
         (fails (if (fboundp 'test-flow--failed-names)
                    (test-flow--failed-names results)
                  (mapcar (lambda (r) (plist-get r :name))
                          (seq-filter (lambda (r) (memq (plist-get r :status) '(fail error)))
                                      (or results '())))))
         (runner (if (and (fboundp 'test-flow--conf) (boundp 'test-flow-runner))
                     (test-flow--conf sess 'runner test-flow-runner)
                   'external-command)))
    (cond
     ((null fails)
      (message "test-flow: no failed tests to run")
      (call-interactively (if (fboundp 'test-flow-run) 'test-flow-run #'ignore)))
     ((eq runner 'in-emacs-ert)
      (if (fboundp 'test-flow--run-failed-in-emacs)
          (test-flow--run-failed-in-emacs sess fails)
        (user-error "test-flow: in-emacs failed-run not available")))
     (t
      (let ((cmd (and (fboundp 'test-flow--build-external-failed-cmd)
                      (test-flow--build-external-failed-cmd sess fails))))
        (if (not cmd)
            (progn
              (message "test-flow: cannot run failed selectively; running all")
              (call-interactively (if (fboundp 'test-flow-run) 'test-flow-run #'ignore)))
          (if (fboundp 'test-flow--maybe-start-run)
              (test-flow--maybe-start-run sess cmd "failed")
            (user-error "test-flow: external runner not available"))))))))

(defun test-flow-runner--impl-dump-concurrency ()
  "Dump current concurrency state to *Messages*."
  (interactive)
  (let ((test-flow-log-enabled t))
    (if (fboundp 'test-flow--log-concurrency-state)
        (test-flow--log-concurrency-state)
      (message "[test-flow] concurrency: (no state)"))))

(defun test-flow-runner--impl-detect-runner ()
  "Detect a suitable external command for running tests and set it per-session.

Heuristics (in order):
- tests/run-tests.el or test/run-tests.el → emacs -Q --batch -l <path>
- flake.nix:
  - apps.tests → nix run .#tests
  - checks.<name> (ert/tests) → nix build .#checks.<system>.<name>
  - otherwise → nix flake check (fallback)
- Cask present → cask exec ert-runner

If multiple candidates are available, prompt to choose."
  (interactive)
  (let* ((root (if (fboundp 'test-flow--project-root)
                   (test-flow--project-root)
                 default-directory))
         (sess (and (fboundp 'test-flow--get-session) (test-flow--get-session root)))
         (json (expand-file-name "tests/run-tests.el" root))
         (json2 (expand-file-name "test/run-tests.el" root))
         (flake (expand-file-name "flake.nix" root))
         (cask  (expand-file-name "Cask" root))
         (cands nil))
    ;; Direct entrypoints
    (when (file-exists-p json)
      (push (cons "emacs -Q --batch -l tests/run-tests.el"
                  (list "emacs" "-Q" "--batch" "-l" json))
            cands))
    (when (and (not (file-exists-p json)) (file-exists-p json2))
      (push (cons "emacs -Q --batch -l test/run-tests.el"
                  (list "emacs" "-Q" "--batch" "-l" json2))
            cands))
    ;; Flake-based entrypoints
    (when (file-exists-p flake)
      (let* ((s (with-temp-buffer
                  (insert-file-contents-literally flake)
                  (buffer-string)))
             (sys (if (fboundp 'test-flow--nix-current-system)
                      (test-flow--nix-current-system)
                    "x86_64-linux"))
             (has-apps-tests (string-match-p "apps\\(?:.\\|\n\\)*?tests[ \t]*=" s))
             (has-checks-ert (string-match-p "checks\\(?:.\\|\n\\)*?ert[ \t]*=" s))
             (has-checks-tests (string-match-p "checks\\(?:.\\|\n\\)*?tests[ \t]*=" s)))
        (when has-apps-tests
          (push (cons "nix run .#tests" (list "nix" "run" ".#tests")) cands))
        (when has-checks-ert
          (push (cons (format "nix build -L --no-link .#checks.%s.ert" sys)
                      (list "nix" "build" "-L" "--print-build-logs" "--no-link" "--rebuild" (format ".#checks.%s.ert" sys)))
                cands))
        (when has-checks-tests
          (push (cons (format "nix build -L --no-link .#checks.%s.tests" sys)
                      (list "nix" "build" "-L" "--print-build-logs" "--no-link" "--rebuild" (format ".#checks.%s.tests" sys)))
                cands))
        ;; Fallback (heavy) if nothing specific matched
        (when (and (not has-apps-tests) (not has-checks-ert) (not has-checks-tests))
          (push (cons "nix flake check" (list "nix" "flake" "check")) cands))))
    ;; Cask runner
    (when (file-exists-p cask)
      (push (cons "cask exec ert-runner"
                  (list "cask" "exec" "ert-runner"))
            cands))
    (cond
     ((null cands)
      (user-error "test-flow: no known test entrypoint found under %s" root))
     ((= (length cands) 1)
      (when (fboundp 'test-flow--set-conf)
        (test-flow--set-conf sess 'external-command (cdar cands)))
      (when (boundp 'test-flow-external-command)
        (setq test-flow-external-command (cdar cands)))
      (message "test-flow: session external command set to %S" (cdar cands)))
     (t
      (let* ((choice (completing-read "Choose runner: " (mapcar #'car cands) nil t))
             (cmd (cdr (assoc choice cands))))
        (when (fboundp 'test-flow--set-conf)
          (test-flow--set-conf sess 'external-command cmd))
        (when (boundp 'test-flow-external-command)
          (setq test-flow-external-command cmd))
        (message "test-flow: session external command set to %S" cmd))))))

;; Ensure public commands resolve to modular implementations (override wrappers).
(defalias 'test-flow-run             'test-flow-runner--impl-run)
(defalias 'test-flow-run-failed      'test-flow-runner--impl-run-failed)
(defalias 'test-flow-detect-runner   'test-flow-runner--impl-detect-runner)
(defalias 'test-flow-dump-concurrency 'test-flow-runner--impl-dump-concurrency)

;; -----------------------------------------------------------------------------
;; Internal heuristics (migrated from monolith)
;; -----------------------------------------------------------------------------

(defun test-flow-runner--looks-like-batch-output (s)
  "Heuristic: return non-nil if S looks like ERT batch output."
  (and (stringp s)
       (or (string-match-p "^Running[ \t]+[0-9]+[ \t]+tests?" s)
           (string-match-p "^Ran[ \t]+[0-9]+[ \t]+tests?" s)
           (string-match-p "^Test[ \t]+" s)
           (string-match-p "^[ \t]*\\(FAILED\\|ERROR\\|SKIPPED\\|XFAIL\\|XPASS\\)[ \t]" s)
           (string-match-p "^[ \t]*passed[ \t]+[0-9]+/[0-9]+[ \t]+" s))))

(defun test-flow-runner--looks-like-json-output (s)
  "Heuristic: return non-nil if S looks like our JSON payload."
  (and (stringp s)
       (string-match-p "{" s)
       (or (string-match-p "\"tests\"" s)
           (string-match-p "\"summary\"" s))))

(defun test-flow-runner--choose-output-for-parse (stdout stderr)
  "Choose stream to parse using simple heuristics.

Prefer JSON when only one side looks like JSON.
If both sides look like ERT batch output, merge them (stdout + stderr),
because ERT often splits progress and failures across streams.
Otherwise, fall back to non-empty stdout, then stderr."
  (cond
   ;; Prefer JSON stream when only one side looks like JSON
   ((and (test-flow-runner--looks-like-json-output stdout)
         (not (test-flow-runner--looks-like-json-output stderr)))
    stdout)
   ((and (test-flow-runner--looks-like-json-output stderr)
         (not (test-flow-runner--looks-like-json-output stdout)))
    stderr)
   ;; If both look like batch output → merge
   ((and (test-flow-runner--looks-like-batch-output stdout)
         (test-flow-runner--looks-like-batch-output stderr))
    (concat (or stdout "") "\n" (or stderr "")))
   ;; Otherwise prefer the side that looks like ERT batch output
   ((and (test-flow-runner--looks-like-batch-output stdout)
         (not (test-flow-runner--looks-like-batch-output stderr)))
    stdout)
   ((and (test-flow-runner--looks-like-batch-output stderr)
         (not (test-flow-runner--looks-like-batch-output stdout)))
    stderr)
   ;; Fallbacks
   ((and (stringp stdout) (> (length stdout) 0)) stdout)
   ((and (stringp stderr) (> (length stderr) 0)) stderr)
   (t "")))

;; -----------------------------------------------------------------------------
;; Concurrency queue and starters (migrated from monolith)
;; -----------------------------------------------------------------------------

;; Share state with monolith
(defvar test-flow--active-run-count 0)
(defvar test-flow--run-queue nil)

;; Extra declarations used by the migrated functions
(declare-function test-flow-open-panel "test-flow-panel" ())
(declare-function test-flow--session-root "test-flow-core" (sess))
(declare-function test-flow--session-panel-name "test-flow-core" (root))
(declare-function test-flow--set-conf "test-flow-core" (sess key value))
(declare-function test-flow--session-process "test-flow-core" (sess))
(declare-function test-flow--set-last-raw-output "test-flow-core" (sess value))
(declare-function test-flow--set-last-stderr-output "test-flow-core" (sess value))
(declare-function test-flow--set-process "test-flow-core" (sess value))
(declare-function test-flow--get-last-activity-at "test-flow-core" (sess))
(declare-function test-flow--set-last-activity-at "test-flow-core" (sess value))
(declare-function test-flow--set-last-summary "test-flow-core" (sess value))
(declare-function test-flow--set-last-results "test-flow-core" (sess value))
(declare-function test-flow--set-last-parser "test-flow-core" (sess value))
(declare-function test-flow--touch-session "test-flow-core" (sess))
(declare-function test-flow--render "test-flow-render" ())

(defun test-flow-runner--enqueue-run (label root thunk &optional cmd)
  "Enqueue THUNK with LABEL and ROOT (and optional CMD) for later execution."
  (let ((item (list :thunk thunk :label label :root root :cmd cmd)))
    (setq test-flow--run-queue (append test-flow--run-queue (list item)))))

(defun test-flow-runner--dequeue-run ()
  "Dequeue and return next run item plist or nil."
  (let ((head (car test-flow--run-queue)))
    (setq test-flow--run-queue (cdr test-flow--run-queue))
    head))

(defun test-flow-runner--log-concurrency-state ()
  "Log snapshot of concurrency: active processes and queued items."
  (when (boundp 'test-flow-log-enabled)
    (let (actives)
      (maphash
       (lambda (_ s)
         (let ((p (and (fboundp 'test-flow--session-process)
                       (test-flow--session-process s))))
           (when (and p (process-live-p p))
             (push (format "%s label=%s pid=%s cmd=%S"
                           (and (fboundp 'test-flow--session-root)
                                (test-flow--session-root s))
                           (or (process-get p 'test-flow-label) "-")
                           (ignore-errors (process-id p))
                           (process-get p 'test-flow-cmd))
                   actives))))
       (and (boundp 'test-flow--sessions) test-flow--sessions))
      (when (fboundp 'test-flow--log)
        (test-flow--log "state: active=%d queued=%d" test-flow--active-run-count (length test-flow--run-queue)))
      (when actives
        (dolist (ln (nreverse actives))
          (when (fboundp 'test-flow--log) (test-flow--log "active: %s" ln))))
      (when test-flow--run-queue
        (let ((i 0))
          (dolist (item test-flow--run-queue)
            (setq i (1+ i))
            (when (fboundp 'test-flow--log)
              (test-flow--log "queued[%d]: root=%s label=%s cmd=%S"
                              i (plist-get item :root) (plist-get item :label) (plist-get item :cmd)))))))))

(defun test-flow-runner--finish-run ()
  "Bookkeeping after a run finishes: free a slot and start next queued run."
  (when (> test-flow--active-run-count 0)
    (cl-decf test-flow--active-run-count))
  (when (fboundp 'test-flow--log)
    (test-flow--log "finish: freed slot → active=%d queued=%d" test-flow--active-run-count (length test-flow--run-queue)))
  (let ((next (test-flow-runner--dequeue-run)))
    (when next
      (cl-incf test-flow--active-run-count)
      (when (fboundp 'test-flow--log)
        (test-flow--log "dequeue: start %s active=%d queued=%d root=%s"
                        (or (plist-get next :label) "?")
                        test-flow--active-run-count
                        (length test-flow--run-queue)
                        (or (plist-get next :root) "?")))
      (funcall (plist-get next :thunk))))
  (test-flow-runner--log-concurrency-state))

(defun test-flow-runner--maybe-start-thunk (sess label thunk)
  "Start THUNK respecting concurrency for SESS and LABEL."
  (let ((root (and (fboundp 'test-flow--session-root) (test-flow--session-root sess))))
    (if (>= test-flow--active-run-count (if (boundp 'test-flow-max-concurrent-runs) test-flow-max-concurrent-runs 3))
        (progn
          (when (fboundp 'test-flow--log)
            (test-flow--log "queue(thunk): %s (limit %d reached) active=%d queued=%d root=%s"
                            label (if (boundp 'test-flow-max-concurrent-runs) test-flow-max-concurrent-runs 3)
                            test-flow--active-run-count
                            (length test-flow--run-queue) root))
          (test-flow-runner--enqueue-run label root thunk nil)
          (when (fboundp 'test-flow-open-panel) (test-flow-open-panel))
          (when (and (fboundp 'test-flow--session-panel-name) (fboundp 'test-flow--session-root))
            (with-current-buffer (get-buffer-create (test-flow--session-panel-name root))
              (let ((inhibit-read-only t))
                (goto-char (point-max))
                (insert (propertize (format "Queued: %s\n" label) 'face 'shadow)))))
          (test-flow-runner--log-concurrency-state))
      (cl-incf test-flow--active-run-count)
      (when (fboundp 'test-flow--log)
        (test-flow--log "start(thunk): %s active=%d→%d queued=%d root=%s"
                        label (1- test-flow--active-run-count) test-flow--active-run-count
                        (length test-flow--run-queue) root))
      (funcall thunk)
      (test-flow-runner--log-concurrency-state))))

(defun test-flow-runner--maybe-start-run (sess cmd label)
  "Start CMD for SESS with LABEL respecting concurrency limit."
  (let ((root (and (fboundp 'test-flow--session-root) (test-flow--session-root sess))))
    (if (>= test-flow--active-run-count (if (boundp 'test-flow-max-concurrent-runs) test-flow-max-concurrent-runs 3))
        (progn
          (when (fboundp 'test-flow--log)
            (test-flow--log "queue: %s (limit %d reached) active=%d queued=%d root=%s"
                            label (if (boundp 'test-flow-max-concurrent-runs) test-flow-max-concurrent-runs 3)
                            test-flow--active-run-count
                            (length test-flow--run-queue)
                            root))
          (test-flow-runner--enqueue-run label root (lambda () (test-flow-runner--start-run-internal sess cmd label)) cmd)
          (when (fboundp 'test-flow-open-panel) (test-flow-open-panel))
          (when (and (fboundp 'test-flow--session-panel-name) root)
            (with-current-buffer (get-buffer-create (test-flow--session-panel-name root))
              (let ((inhibit-read-only t))
                (goto-char (point-max))
                (insert (propertize (format "Queued: %s\n" label) 'face 'shadow)))))
          (test-flow-runner--log-concurrency-state))
      (cl-incf test-flow--active-run-count)
      (when (fboundp 'test-flow--log)
        (test-flow--log "start: %s active=%d→%d queued=%d root=%s"
                        label (1- test-flow--active-run-count) test-flow--active-run-count
                        (length test-flow--run-queue) root))
      (test-flow-runner--start-run-internal sess cmd label)
      (test-flow-runner--log-concurrency-state))))

(defun test-flow-runner--start-run-internal (sess cmd label)
  "Actually start the external process CMD for SESS, annotating LABEL."
  (when (fboundp 'test-flow--touch-session) (test-flow--touch-session sess))
  (run-hook-with-args 'test-flow-before-run-hook sess)
  (let* ((root (and (fboundp 'test-flow--session-root) (test-flow--session-root sess)))
         (panel (and (fboundp 'test-flow--session-panel-name) (test-flow--session-panel-name root))))
    (when (and (fboundp 'test-flow--session-process)
               (process-live-p (test-flow--session-process sess)))
      (when (fboundp 'test-flow--log) (test-flow--log "Killing previous process..."))
      (ignore-errors (kill-process (test-flow--session-process sess))))
    (when (fboundp 'test-flow--set-last-raw-output) (test-flow--set-last-raw-output sess nil))
    (when (fboundp 'test-flow--set-last-stderr-output) (test-flow--set-last-stderr-output sess nil))
    (when (fboundp 'test-flow-open-panel) (test-flow-open-panel))
    (when panel
      (with-current-buffer (get-buffer-create panel)
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (insert (propertize (format "Running: %s %S\n" label cmd) 'face 'shadow)))))
    (let ((default-directory (or root default-directory)))
      (when (fboundp 'test-flow--log)
        (test-flow--log "spawn: root=%s cmd=%S label=%s" root cmd label))
      (let* ((stdout-buf (generate-new-buffer " *test-flow-stdout*"))
             (stderr-buf (generate-new-buffer " *test-flow-stderr*"))
             (seq (1+ (or (and (fboundp 'test-flow--conf) (test-flow--conf sess 'run-seq 0)) 0))))
        (when (fboundp 'test-flow--set-conf) (test-flow--set-conf sess 'run-seq seq))
        (let* ((p (make-process
                   :name "test-flow-runner"
                   :command cmd
                   :noquery t
                   :buffer stdout-buf
                   :connection-type 'pipe
                   :sentinel #'test-flow--proc-sentinel
                   :stderr stderr-buf)))
          (process-put p 'test-flow-session sess)
          (process-put p 'test-flow-stdout-buf stdout-buf)
          (process-put p 'test-flow-stderr-buf stderr-buf)
          (process-put p 'test-flow-label label)
          (process-put p 'test-flow-root root)
          (process-put p 'test-flow-cmd cmd)
          (process-put p 'test-flow-run-seq seq)
          (when (fboundp 'test-flow--set-process) (test-flow--set-process sess p))
          (when (boundp 'test-flow--process) (setq test-flow--process p))
          (test-flow-runner--log-concurrency-state))))))

;; -----------------------------------------------------------------------------
;; Process filter and sentinel (migrated from monolith)
;; -----------------------------------------------------------------------------

;; Declarations used by migrated functions
(declare-function test-flow--get-last-raw-output "test-flow-core" (sess))
(declare-function test-flow--set-last-raw-output "test-flow-core" (sess value))
(declare-function test-flow--set-last-stderr-output "test-flow-core" (sess value))
(declare-function test-flow--get-process "test-flow-core" (sess))
(declare-function test-flow--set-process "test-flow-core" (sess value))
(declare-function test-flow--session-panel-name "test-flow-core" (root))
(declare-function test-flow--session-root "test-flow-core" (sess))
(declare-function test-flow--touch-session "test-flow-core" (sess))
(declare-function test-flow--get-last-summary "test-flow-core" (sess))
(declare-function test-flow--get-last-results "test-flow-core" (sess))
(declare-function test-flow--set-last-summary "test-flow-core" (sess value))
(declare-function test-flow--set-last-results "test-flow-core" (sess value))
(declare-function test-flow--set-last-parser "test-flow-core" (sess value))
(declare-function test-flow--render "test-flow-render" ())
(declare-function test-flow--session-list "test-flow-core" ())

;; Parser delegates (monolith already delegates these to test-flow-parse)
(declare-function test-flow--parse-json-output "test-flow-parse" (out))
(declare-function test-flow--parse-batch-output "test-flow-parse" (out))
(declare-function test-flow--conf "test-flow-core" (sess key default))

(defvar test-flow-max-raw-output-bytes)
(defvar test-flow-parser)
(defvar test-flow--last-summary nil)
(defvar test-flow--last-results nil)
(defvar test-flow--last-raw-output nil)
(defvar test-flow--process nil)

(defun test-flow-runner--proc-filter (proc chunk)
  "Accumulate CHUNK from the running process, session-aware."
  (condition-case err
      (let* ((sess (process-get proc 'test-flow-session))
             (old (and sess (fboundp 'test-flow--get-last-raw-output)
                       (test-flow--get-last-raw-output sess)))
             (combined (concat (or old "") chunk)))
        ;; Trim per configured cap to save memory
        (when (and (boundp 'test-flow-max-raw-output-bytes)
                   (integerp test-flow-max-raw-output-bytes)
                   (> (length combined) test-flow-max-raw-output-bytes))
          (setq combined (substring combined (- (length combined) test-flow-max-raw-output-bytes))))
        (when (and sess (fboundp 'test-flow--set-last-raw-output))
          (test-flow--set-last-raw-output sess combined))
        (setq test-flow--last-raw-output combined))
    (error
     (when (fboundp 'test-flow--log)
       (test-flow--log "Filter error: %S" err)))))

(defun test-flow-runner--sentinel-flush ()
  "Give the process filter a brief chance to flush remaining output."
  (dotimes (_ 3) (accept-process-output nil 0.05)))

(defun test-flow-runner--sentinel-read-streams (proc)
  "Return plist with session and streams for PROC: (:sess :root :stdout :stderr :stdout-buf :stderr-buf)."
  (let* ((sess (process-get proc 'test-flow-session))
         (root (and (fboundp 'test-flow--session-root) (test-flow--session-root sess)))
         (stdout-buf (or (process-get proc 'test-flow-stdout-buf)
                         (process-buffer proc)))
         (stderr-buf (process-get proc 'test-flow-stderr-buf))
         (stdout (when (buffer-live-p stdout-buf)
                   (with-current-buffer stdout-buf (buffer-string))))
         (stderr-str (when (buffer-live-p stderr-buf)
                       (with-current-buffer stderr-buf (buffer-string)))))
    (list :sess sess :root root
          :stdout stdout :stderr stderr-str
          :stdout-buf stdout-buf :stderr-buf stderr-buf)))

(defun test-flow-runner--sentinel-parse (sess raw)
  "Parse RAW according to SESS parser preference. Return (used summary results)."
  (let* ((pmode (and (fboundp 'test-flow--conf)
                     (test-flow--conf sess 'parser (and (boundp 'test-flow-parser) test-flow-parser))))
         used summary results)
    (pcase pmode
      ('json
       (setq used 'json)
       (let ((pair (and (fboundp 'test-flow--parse-json-output)
                        (test-flow--parse-json-output raw))))
         (when pair (setq summary (car pair) results (cdr pair)))))
      ('ert-batch
       (setq used 'ert-batch)
       (let ((pair (and (fboundp 'test-flow--parse-batch-output)
                        (test-flow--parse-batch-output raw))))
         (setq summary (car pair) results (cdr pair))))
      (_
       (let ((pair (and (fboundp 'test-flow--parse-json-output)
                        (test-flow--parse-json-output raw))))
         (if pair
             (progn
               (setq used 'json summary (car pair) results (cdr pair)))
           (setq used 'ert-batch)
           (let ((p2 (and (fboundp 'test-flow--parse-batch-output)
                          (test-flow--parse-batch-output raw))))
             (setq summary (car p2) results (cdr p2)))))))
    (list used summary results)))

(defun test-flow-runner--sentinel-store (sess used summary results stdout-str stderr-str)
  "Store RESULTS and SUMMARY into SESS (and globals), trimming STDOUT-STR/STDERR-STR."
  (when (and sess (fboundp 'test-flow--set-last-raw-output))
    ;; Trim stdout/stderr tails to cap memory
    (when (and (stringp stdout-str)
               (boundp 'test-flow-max-raw-output-bytes)
               (integerp test-flow-max-raw-output-bytes)
               (> (length stdout-str) test-flow-max-raw-output-bytes))
      (setq stdout-str (substring stdout-str (- (length stdout-str) test-flow-max-raw-output-bytes))))
    (when (and (stringp stderr-str)
               (boundp 'test-flow-max-raw-output-bytes)
               (integerp test-flow-max-raw-output-bytes)
               (> (length stderr-str) test-flow-max-raw-output-bytes))
      (setq stderr-str (substring stderr-str (- (length stderr-str) test-flow-max-raw-output-bytes))))
    (test-flow--set-last-raw-output sess stdout-str)
    (when (fboundp 'test-flow--set-last-stderr-output)
      (test-flow--set-last-stderr-output sess stderr-str))
    (when (fboundp 'test-flow--touch-session) (test-flow--touch-session sess))
    (when (fboundp 'test-flow--set-last-summary) (test-flow--set-last-summary sess summary))
    (when (fboundp 'test-flow--set-last-results) (test-flow--set-last-results sess results))
    (when (fboundp 'test-flow--set-process) (test-flow--set-process sess nil))
    (when (fboundp 'test-flow--set-last-parser) (test-flow--set-last-parser sess used)))
  (setq test-flow--last-summary summary
        test-flow--last-results results
        test-flow--last-raw-output stdout-str))

(defun test-flow-runner--sentinel-render (root)
  "Re-render panel for ROOT."
  (let* ((bufname (and (fboundp 'test-flow--session-panel-name)
                       (test-flow--session-panel-name (or root default-directory)))))
    (let ((test-flow--panel-buffer-name bufname))
      (when (fboundp 'test-flow--render) (test-flow--render)))))

(defun test-flow-runner--proc-sentinel (proc event)
  "Handle process EVENT (session-aware)."
  (condition-case err
      (progn
        (when (fboundp 'test-flow--log)
          (test-flow--log "Sentinel: %s" (string-trim (or event ""))))
        (when (and (stringp event)
                   (string-match-p "\\(finished\\|exited\\)" event))
          (test-flow-runner--sentinel-flush)
          (pcase-let* ((plist (test-flow-runner--sentinel-read-streams proc))
                       (sess (plist-get plist :sess))
                       (root (plist-get plist :root))
                       (stdout (plist-get plist :stdout))
                       (stderr-str (plist-get plist :stderr))
                       (stdout-buf (plist-get plist :stdout-buf))
                       (stderr-buf (plist-get plist :stderr-buf))
                       (raw (test-flow-runner--choose-output-for-parse stdout stderr-str)))
            (when (fboundp 'test-flow--log)
              (test-flow--log "sentinel: root=%s label=%s" root (process-get proc 'test-flow-label))
              (test-flow--log "sentinel: stdout len=%s stderr len=%s → using=%s"
                              (if (stringp stdout) (number-to-string (length stdout)) "nil")
                              (if (stringp stderr-str) (number-to-string (length stderr-str)) "nil")
                              (if (eq raw stdout) "stdout" (if (eq raw stderr-str) "stderr" "empty"))))
            ;; Drop stale (older) runs finishing after a newer one started
            (let* ((seq (process-get proc 'test-flow-run-seq))
                   (latest (and sess (fboundp 'test-flow--conf) (test-flow--conf sess 'run-seq 0)))
                   (outdated (and (numberp seq) (numberp latest) (< seq latest))))
              (if outdated
                  (progn
                    (when (fboundp 'test-flow--log)
                      (test-flow--log "sentinel: dropping outdated results: seq=%s latest=%s" seq latest))
                    (when (buffer-live-p stdout-buf) (kill-buffer stdout-buf))
                    (when (buffer-live-p stderr-buf) (kill-buffer stderr-buf))
                    (test-flow-runner--finish-run))
                (pcase-let ((`(,used ,summary ,results) (test-flow-runner--sentinel-parse sess raw)))
                  (when (fboundp 'test-flow--log)
                    (test-flow--log "sentinel parsed: results=%d total=%s (parser=%s)"
                                    (length results)
                                    (or (and (listp summary) (alist-get 'total summary)) "?")
                                    (or used (and (fboundp 'test-flow--conf)
                                                  (test-flow--conf sess 'parser (and (boundp 'test-flow-parser) test-flow-parser))))))
                  ;; If exit was abnormal but F+E==0 (and U>0), dump raw head to diagnose parser miss.
                  (let* ((f (or (and (listp summary) (alist-get 'failed summary)) 0))
                         (e (or (and (listp summary) (alist-get 'error summary)) 0))
                         (u (or (and (listp summary) (alist-get 'unexpected summary)) 0)))
                    (when (and (string-match-p "\\(finished\\|exited\\)" (or event ""))
                               (string-match-p "abnormally\\|exited" (or event ""))
                               (= (+ f e) 0)
                               (> u 0))
                      (let ((head (and (stringp raw) (substring raw 0 (min 400 (length raw))))))
                        (when (fboundp 'test-flow--log)
                          (test-flow--log "anomaly: exit!=0 but F+E=0 (U=%d). raw head:\n%s" u (or head ""))))))
                  (test-flow-runner--sentinel-store sess used summary results stdout stderr-str)
                  (run-hook-with-args 'test-flow-after-run-hook sess summary results)
                  (when (buffer-live-p stdout-buf) (kill-buffer stdout-buf))
                  (when (buffer-live-p stderr-buf) (kill-buffer stderr-buf))
                  (test-flow-runner--sentinel-render root)
                  (test-flow-runner--finish-run)))))))
    (error
     (when (fboundp 'test-flow--log)
       (test-flow--log "Sentinel error: %S" err)))))

;;; Compatibility shim (monolith-era internal)
(unless (fboundp 'test-flow--choose-output-for-parse)
  (defalias 'test-flow--choose-output-for-parse
    'test-flow-runner--choose-output-for-parse))

;; Provide shims for modular concurrency starters used by public impls.
(unless (fboundp 'test-flow--maybe-start-run)
  (defalias 'test-flow--maybe-start-run 'test-flow-runner--maybe-start-run))
(unless (fboundp 'test-flow--maybe-start-thunk)
  (defalias 'test-flow--maybe-start-thunk 'test-flow-runner--maybe-start-thunk))
(unless (fboundp 'test-flow--start-run-internal)
  (defalias 'test-flow--start-run-internal 'test-flow-runner--start-run-internal))

;; Ensure monolith-era sentinel/filter names resolve to modular implementations
(unless (fboundp 'test-flow--proc-sentinel)
  (defalias 'test-flow--proc-sentinel 'test-flow-runner--proc-sentinel))

(unless (fboundp 'test-flow--proc-filter)
  (defalias 'test-flow--proc-filter 'test-flow-runner--proc-filter))

;; Provide a safe modular restart to avoid depending on monolith implementations.
;;;###autoload
(defun test-flow-restart ()
  "Fully restart test-flow after code reload (modular, safe).

Stops all sessions and processes, disables watchers, cancels per-session timers,
clears global queues, and re-opens the panel for the current project."
  (interactive)
  (condition-case err
      (progn
        (when (fboundp 'test-flow--log)
          (test-flow--log "restart: begin"))
        (let ((cur-root (and (fboundp 'test-flow--project-root) (test-flow--project-root))))
          ;; Stop all sessions (kill processes, disable watchers, cancel per-session timers)
          (when (and (boundp 'test-flow--sessions) (hash-table-p test-flow--sessions))
            (maphash
             (lambda (_root s)
               ;; Kill process if live
               (when (and (fboundp 'test-flow--session-process)
                          (process-live-p (test-flow--session-process s)))
                 (ignore-errors (kill-process (test-flow--session-process s))))
               ;; Disable watch (optional module)
               (when (fboundp 'test-flow-watch--disable-watch)
                 (ignore-errors (test-flow-watch--disable-watch s)))
               ;; Cancel debounce timer
               (when (timerp (ignore-errors (test-flow--session-debounce-timer s)))
                 (cancel-timer (test-flow--session-debounce-timer s))
                 (ignore-errors (setf (test-flow--session-debounce-timer s) nil))))
             test-flow--sessions)
            ;; Clear registry but keep the hash-table object (avoid hash-table-p nil errors)
            (clrhash test-flow--sessions))
          ;; Reset concurrency/queues
          (setq test-flow--run-queue nil
                test-flow--active-run-count 0)
          ;; Re-open panel for current project
          (when (and cur-root (fboundp 'test-flow-open-panel))
            (let ((default-directory cur-root))
              (test-flow-open-panel)))
          (message "test-flow: restarted")))
    (error
     (when (fboundp 'test-flow--log)
       (test-flow--log "restart error: %S" err))
     (user-error "test-flow: restart failed: %S" err))))

(provide 'test-flow-runner)
;;; test-flow-runner.el ends here
