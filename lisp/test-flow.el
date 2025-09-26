;;; test-flow.el --- Umbrella for test-flow -*- lexical-binding: t; -*-
;; Author: Peter Kosov <11111000000@email.com>
;; Version: 0.1.1
;; Package-Requires: ((emacs "27.1"))
;; Keywords: tools, lisp, testing
;; URL: https://github.com/11111000000/test-flow

;;; Commentary:
;; Thin entry point that aggregates the modular implementation.
;; Users and tests can `(require 'test-flow)' as before.

;;; Code:

(require 'test-flow-modular)

(provide 'test-flow)
;;; test-flow.el ends here
