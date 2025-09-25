;;; install-elpa.el --- Install required ELPA packages for project (batch) -*- lexical-binding: t -*-

;; Usage:
;;   export SSL_CERT_FILE=/run/current-system/sw/etc/ssl/certs/ca-bundle.crt
;;   emacs -Q --batch -l scripts/install-elpa.el

(require 'package)

(setq package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(let ((pkgs '(package-build undercover dash shut-up)))
  (dolist (p pkgs)
    (unless (package-installed-p p)
      (condition-case err
          (progn
            (message "Installing %s..." p)
            (package-install p)
            (message "Installed %s" p))
        (error
         (message "Failed to install %s: %S" p err))))))

(message "install-elpa.el: done")
