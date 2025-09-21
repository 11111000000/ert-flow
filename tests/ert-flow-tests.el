;;; ert-flow-tests.el --- Tests for ert-flow -*- lexical-binding: t; -*-

(require 'ert)
(require 'ert-flow)

(defun ert-flow-tests--sample-batch ()
  "Return a sample ERT batch output string."
  (mapconcat
   #'identity
   '("Running 2 tests"
     "Test ns/ok passed."
     "Test ns/f1 backtrace:"
     "  Assertion failed: (= 1 2)"
     "  Backtrace: ..."
     "Ran 2 tests, 1 results were unexpected."
     "FAILED ns/f1"
     "")
   "\n"))

(defun ert-flow-tests--sample-json ()
  "Return a sample JSON output string."
  (concat
   "noise before...\n"
   (json-encode
    `(("summary" . (("total" . 2) ("failed" . 1) ("error" . 0) ("duration_ms" . 12)))
      ("tests" .
       [ (("name" . "ns/ok") ("status" . "pass"))
         (("name" . "ns/f1") ("status" . "fail") ("message" . "boom") ("details" . "Assertion failed")) ])))
   "\ntrailing noise"))

(ert-deftest ert-flow/suite-of ()
  (should (equal (ert-flow--suite-of "acp/parser/x") "acp"))
  (should (equal (ert-flow--suite-of "single") "single"))
  (should (equal (ert-flow--suite-of "") "")))

(ert-deftest ert-flow/normalize-command ()
  (should (equal (ert-flow--normalize-command '("emacs" "-Q"))
                 '("emacs" "-Q")))
  (let ((cmd (ert-flow--normalize-command "echo ok")))
    (should (listp cmd))
    (should (stringp (car cmd)))
    (should (member "-lc" cmd))))

(ert-deftest ert-flow/parse-batch-output ()
  (let* ((out (ert-flow-tests--sample-batch))
         (parsed (ert-flow--parse-batch-output out))
         (summary (car parsed))
         (results (cdr parsed)))
    (should (equal (alist-get 'total summary) 2))
    (should (equal (alist-get 'unexpected summary) 1))
    (should (= (length results) 2))
    (let* ((r-ok (seq-find (lambda (r) (equal (plist-get r :name) "ns/ok")) results))
           (r-f1 (seq-find (lambda (r) (equal (plist-get r :name) "ns/f1")) results)))
      (should (eq (plist-get r-ok :status) 'pass))
      (should (memq (plist-get r-f1 :status) '(fail error)))
      (should (string-match-p "Assertion failed" (or (plist-get r-f1 :details) ""))))))

(ert-deftest ert-flow/parse-json-output ()
  (let* ((out (ert-flow-tests--sample-json))
         (parsed (ert-flow--parse-json-output out)))
    (should parsed)
    (let* ((summary (car parsed))
           (results (cdr parsed)))
      (should (= (alist-get 'total summary) 2))
      (should (= (length results) 2))
      (let ((r-ok (seq-find (lambda (r) (equal (plist-get r :name) "ns/ok")) results))
            (r-f1 (seq-find (lambda (r) (equal (plist-get r :name) "ns/f1")) results)))
        (should (eq (plist-get r-ok :status) 'pass))
        (should (eq (plist-get r-f1 :status) 'fail))
        (should (equal (plist-get r-f1 :message) "boom"))))))

(ert-deftest ert-flow/parse-output-auto ()
  (let ((ert-flow-parser 'auto))
    (let ((json-out (ert-flow-tests--sample-json)))
      (should (ert-flow--parse-output json-out)))
    (let ((batch-out (ert-flow-tests--sample-batch)))
      (should (ert-flow--parse-output batch-out)))))

(ert-deftest ert-flow/file-event-eligible ()
  (let ((ert-flow-watch-include-regexp "\\.el\\'")
        (ert-flow-watch-exclude-regexp "/dist/"))
    (should (ert-flow--file-event-eligible-p "/x/a.el"))
    (should-not (ert-flow--file-event-eligible-p "/x/a.txt"))
    (should-not (ert-flow--file-event-eligible-p "/x/dist/a.el"))))

(ert-deftest ert-flow/collect-dirs-respects-depth-and-exclude ()
  (let* ((root (make-temp-file "ert-flow-root" t))
         (a (expand-file-name "a" root))
         (b (expand-file-name "a/b" root))
         (dist (expand-file-name "dist" root)))
    (unwind-protect
        (progn
          (make-directory a t)
          (make-directory b t)
          (make-directory dist t)
          (let ((ert-flow-watch-exclude-regexp "/dist/"))
            (let ((dirs (ert-flow--collect-dirs root 1)))
              (should (member root dirs))
              (should (member a dirs))
              (should-not (member b dirs))     ;; depth limit
              (should-not (member dist dirs))))) ;; excluded
      (ignore-errors (delete-directory root t)))))

(ert-deftest ert-flow/copy-failures ()
  (let ((ert-flow--last-results
         (list (list :name "ns/ok" :status 'pass :message "ok")
               (list :name "ns/f1" :status 'fail :message "boom" :details "D1\nD2")
               (list :name "ns/e1" :status 'error :message "kaboom" :details "E1"))))
    (let ((kill-ring nil))
      (ert-flow-copy-failures)
      (let ((s (current-kill 0 t)))
        (should (string-match-p "ns/f1: boom" s))
        (should (string-match-p "ns/e1: kaboom" s))
        (should (string-match-p "=== ns/f1 ===" s))))))

(ert-deftest ert-flow/parse-batch-summary-keys ()
  (let* ((out (ert-flow-tests--sample-batch))
         (parsed (ert-flow--parse-batch-output out))
         (summary (car parsed)))
    (dolist (k '(passed failed error skipped))
      (should (assoc k summary)))
    ;; duration-ms may be nil if no time was present; the sample has no numeric time,
    ;; so only check that the key exists (may be nil).
    (should (assoc 'duration-ms summary))))

(ert-deftest ert-flow/parse-json-summary-keys ()
  (let* ((out (ert-flow-tests--sample-json))
         (parsed (ert-flow--parse-json-output out))
         (summary (car parsed)))
    (dolist (k '(passed failed error))
      (should (assoc k summary)))
    (should (assoc 'duration-ms summary))))

(ert-deftest ert-flow/render-smoke ()
  (let* ((root (ert-flow--project-root))
         (sess (ert-flow--get-session root))
         (bufname (ert-flow--session-panel-name root)))
    (setf (ert-flow--session-last-summary sess) '((total . 1) (unexpected . 0)))
    (setf (ert-flow--session-last-results sess)
          (list (list :name "ns/ok" :status 'pass :suite "ns")))
    (with-current-buffer (get-buffer-create bufname)
      (ert-flow-panel-mode) ;; Включаем режим панели для стабильного рендера/хедера
      (let ((ert-flow--panel-buffer-name bufname))
        (ert-flow--render)
        (ert-flow-toggle-all-groups t)
        (let ((s (buffer-string)))
          (should (string-match-p "Status" s))
          (should (string-match-p "ns/ok" s)))))))

(ert-deftest ert-flow/render-header-shows-runner ()
  (let* ((root (ert-flow--project-root))
         (bufname (ert-flow--session-panel-name root)))
    (with-current-buffer (get-buffer-create bufname)
      (let ((ert-flow--panel-buffer-name bufname))
        (ert-flow--render)
        (should (string-match-p "Runner:" (buffer-string)))))))

(ert-deftest ert-flow/render-header-shows-parser ()
  "Header shows Parser: <mode> (auto/json/ert-batch/in-emacs)."
  (let* ((root (ert-flow--project-root))
         (bufname (ert-flow--session-panel-name root)))
    (with-current-buffer (get-buffer-create bufname)
      (let ((ert-flow--panel-buffer-name bufname))
        (ert-flow--render)
        (should (string-match-p "Parser:" (buffer-string)))))))

(ert-deftest ert-flow/panel-tags-filter ()
  "Tags filter narrows results."
  (let* ((root (ert-flow--project-root))
         (sess (ert-flow--get-session root))
         (bufname (ert-flow--session-panel-name root)))
    (setf (ert-flow--session-last-summary sess) '((total . 3)))
    (setf (ert-flow--session-last-results sess)
          (list (list :name "ns/ok" :status 'pass :suite "ns" :tags '("fast" "unit"))
                (list :name "ns/slow" :status 'pass :suite "ns" :tags '("slow"))
                (list :name "ns/none" :status 'pass :suite "ns")))

    (with-current-buffer (get-buffer-create bufname)
      (let ((ert-flow--panel-buffer-name bufname))
        (ert-flow-panel-mode)
        (setq-local ert-flow--panel-tags-filter '("fast"))
        (ert-flow--render)
        (ert-flow-toggle-all-groups t)
        (let ((s (buffer-string)))
          (should (string-match-p "ns/ok" s))
          (should-not (string-match-p "ns/slow" s))
          (should-not (string-match-p "ns/none" s)))))))

(ert-deftest ert-flow/parse-batch-xfail-xpass ()
  "Batch parser recognizes XFAIL as xfail and XPASS as fail."
  (let* ((out (mapconcat
               #'identity
               '("Running 3 tests"
                 "Test ns/ok passed."
                 "Ran 3 tests, 1 results were unexpected."
                 "XFAIL ns/xf"
                 "XPASS ns/xp"
                 "")
               "\n"))
         (parsed (ert-flow--parse-batch-output out))
         (results (cdr parsed)))
    (let ((r-xf (seq-find (lambda (r) (equal (plist-get r :name) "ns/xf")) results))
          (r-xp (seq-find (lambda (r) (equal (plist-get r :name) "ns/xp")) results)))
      (should (eq (plist-get r-xf :status) 'xfail))
      (should (eq (plist-get r-xp :status) 'fail)))))

(ert-deftest ert-flow/copy-includes-stdout-stderr ()
  "Copy failures includes optional STDOUT/STDERR tails when enabled."
  (let* ((root (ert-flow--project-root))
         (sess (ert-flow--get-session root))
         (ert-flow-copy-include-stdout t)
         (ert-flow-copy-include-stderr t)
         (ert-flow-copy-backtrace-limit 64))
    (setf (ert-flow--session-last-results sess)
          (list (list :name "ns/f1" :status 'fail :message "boom" :details "D1\nD2")))
    (setf (ert-flow--session-last-raw-output sess) "stdout-aaa\nbbb\nccc")
    (setf (ert-flow--session-last-stderr-output sess) "stderr-xxx\nyyy\nzzz")
    (let ((kill-ring nil))
      (ert-flow-copy-failures)
      (let ((s (current-kill 0 t)))
        (should (string-match-p "STDOUT tail" s))
        (should (string-match-p "STDERR tail" s))
        (should (string-match-p "=== ns/f1 ===" s))))))

(ert-deftest ert-flow/dashboard-smoke ()
  "Dashboard opens and shows headline."
  (ert-flow-dashboard)
  (let ((buf (get-buffer "*ert-flow: dashboard*")))
    (should buf)
    (with-current-buffer buf
      (should (string-match-p "dashboard" (buffer-string))))))

(ert-deftest ert-flow/parse-batch-progress-lines ()
  "Parser recognizes progress-style lines like 'passed 1/2 NAME'."
  (let* ((out (mapconcat
               #'identity
               '("Running 2 tests"
                 "passed 1/2 ns/ok"
                 "passed 2/2 ns/ok2"
                 "Ran 2 tests, 0 results were unexpected."
                 "")
               "\n"))
         (parsed (ert-flow--parse-batch-output out))
         (summary (car parsed))
         (results (cdr parsed)))
    (should (= (alist-get 'total summary) 2))
    (should (= (length results) 2))
    (dolist (nm '("ns/ok" "ns/ok2"))
      (let ((r (seq-find (lambda (x) (equal (plist-get x :name) nm)) results)))
        (should r)
        (should (eq (plist-get r :status) 'pass))))))

(ert-deftest ert-flow/choose-output-fallback-stderr ()
  "When stdout is empty and stderr has batch output, use stderr for parsing."
  (let* ((stderr (ert-flow-tests--sample-batch))
         (raw (ert-flow--choose-output-for-parse nil stderr))
         (parsed (ert-flow--parse-output raw))
         (summary (car parsed))
         (results (cdr parsed)))
    (should (= (alist-get 'total summary) 2))
    (should (>= (length results) 1))
    (let ((r (seq-find (lambda (x) (equal (plist-get x :name) "ns/ok")) results)))
      (should r)
      (should (eq (plist-get r :status) 'pass)))))

(ert-deftest ert-flow/panel-filters-reduce-results ()
  "Panel filters (status/name) reduce visible tests as expected."
  (let* ((root (ert-flow--project-root))
         (sess (ert-flow--get-session root))
         (bufname (ert-flow--session-panel-name root)))
    ;; Seed results: one pass, one fail
    (setf (ert-flow--session-last-summary sess) '((total . 2) (unexpected . 1)))
    (setf (ert-flow--session-last-results sess)
          (list (list :name "ns/ok" :status 'pass :suite "ns")
                (list :name "ns/f1" :status 'fail :suite "ns")))
    (with-current-buffer (get-buffer-create bufname)
      (let ((ert-flow--panel-buffer-name bufname))
        (ert-flow-panel-mode)
        ;; Show all
        (ert-flow--render)
        (ert-flow-toggle-all-groups t)
        (let ((s (buffer-string)))
          (should (string-match-p "ns/ok" s))
          (should (string-match-p "ns/f1" s)))
        ;; Filter to pass only
        (setq-local ert-flow--panel-status-filter '(pass))
        (ert-flow--render)
        (ert-flow-toggle-all-groups t)
        (let ((s (buffer-string)))
          (should (string-match-p "ns/ok" s))
          (should-not (string-match-p "ns/f1" s)))
        ;; Name filter to ns/ok2 (no matches)
        (setq-local ert-flow--panel-name-regexp "ns/ok2")
        (ert-flow--render)
        (ert-flow-toggle-all-groups t)
        (let ((s (buffer-string)))
          (should-not (string-match-p "ns/ok" s))
          (should-not (string-match-p "ns/f1" s)))))))

(ert-deftest ert-flow/toggle-logging ()
  "Toggle logging command flips the flag."
  (let ((ert-flow-log-enabled nil))
    (ert-flow-toggle-logging)
    (should ert-flow-log-enabled)
    (ert-flow-toggle-logging)
    (should-not ert-flow-log-enabled)))

(ert-deftest ert-flow/multi-session-isolation ()
  "Results of different sessions do not mix in their panels."
  (let* ((root-a (make-temp-file "ert-flow-a" t))
         (root-b (make-temp-file "ert-flow-b" t))
         (sess-a (ert-flow--get-session root-a))
         (sess-b (ert-flow--get-session root-b))
         (buf-a (ert-flow--session-panel-name root-a))
         (buf-b (ert-flow--session-panel-name root-b)))
    (unwind-protect
        (progn
          ;; Seed different results for A and B
          (setf (ert-flow--session-last-summary sess-a) '((total . 1)))
          (setf (ert-flow--session-last-results sess-a)
                (list (list :name "ns/ok-a" :status 'pass :suite "ns")))
          (setf (ert-flow--session-last-summary sess-b) '((total . 1)))
          (setf (ert-flow--session-last-results sess-b)
                (list (list :name "ns/ok-b" :status 'pass :suite "ns")))
          ;; Render A
          (with-current-buffer (get-buffer-create buf-a)
            (let ((ert-flow--panel-buffer-name buf-a))
              (ert-flow--render)
              (ert-flow-toggle-all-groups t)
              (let ((s (buffer-string)))
                (should (string-match-p "ns/ok-a" s))
                (should-not (string-match-p "ns/ok-b" s)))))
          ;; Render B
          (with-current-buffer (get-buffer-create buf-b)
            (let ((ert-flow--panel-buffer-name buf-b))
              (ert-flow--render)
              (ert-flow-toggle-all-groups t)
              (let ((s (buffer-string)))
                (should (string-match-p "ns/ok-b" s))
                (should-not (string-match-p "ns/ok-a" s))))))
      ;; Cleanup temp dirs
      (ignore-errors (delete-directory root-a t))
      (ignore-errors (delete-directory root-b t)))))

(ert-deftest ert-flow/find-panel-session-by-name ()
  "ert-flow--find-panel-session resolves the correct session by panel buffer name."
  (let* ((root (make-temp-file "ert-flow-root" t))
         (sess (ert-flow--get-session root))
         (bufname (ert-flow--session-panel-name root)))
    (unwind-protect
        (with-current-buffer (get-buffer-create bufname)
          (let ((ert-flow--panel-buffer-name bufname))
            (should (eq (ert-flow--find-panel-session) sess))))
      (ignore-errors (delete-directory root t)))))

(ert-deftest ert-flow/watch-toggle-updates-session ()
  "Toggling watch flips session state and reflects in status/header."
  (let* ((root (ert-flow--project-root))
         (sess (ert-flow--get-session root))
         (bufname (ert-flow--session-panel-name root)))
    (with-current-buffer (get-buffer-create bufname)
      (let ((ert-flow--panel-buffer-name bufname))
        (ert-flow-panel-mode)
        (ert-flow--render)
        (let ((initial (ert-flow--session-watch-enabled sess)))
          (ert-flow-toggle-watch)
          (should (not (eq (ert-flow--session-watch-enabled sess) initial)))
          ;; Should appear in Status block
          (let ((s (buffer-string)))
            (should (string-match-p "Watch: \\(On\\|Off\\)" s)))
          ;; Header-line может быть отключён средой (пустая строка в TTY/CI).
          ;; Считаем допустимым либо наличие метки, либо пустую строку.
          (let ((hdr (format-mode-line header-line-format)))
            (should (or (string-match-p "Watch: \\(On\\|Off\\)" hdr)
                        (string= hdr ""))))
          ;; Flip back to restore
          (ert-flow-toggle-watch)
          (should (eq (ert-flow--session-watch-enabled sess) initial)))))))

(ert-deftest ert-flow/watch-state-fn-reads-current-session ()
  "ert-flow-view-controls--watch-state returns ON after enabling."
  (let* ((root (ert-flow--project-root))
         (sess (ert-flow--get-session root))
         (bufname (ert-flow--session-panel-name root)))
    (with-current-buffer (get-buffer-create bufname)
      (let ((ert-flow--panel-buffer-name bufname))
        (ert-flow-panel-mode)
        (ert-flow--render)
        ;; Ensure off, then on
        (when (ert-flow--session-watch-enabled sess)
          (ert-flow-toggle-watch))
        (should (eq (ert-flow-view-controls--watch-state) 'off))
        (ert-flow-toggle-watch)
        (should (eq (ert-flow-view-controls--watch-state) 'on))
        ;; cleanup: turn off
        (ert-flow-toggle-watch)))))

(provide 'ert-flow-tests)
;;; ert-flow-tests.el ends here
