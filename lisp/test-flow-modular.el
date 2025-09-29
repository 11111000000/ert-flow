;;; test-flow-modular.el --- Modular entry aggregator for test-flow  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Aggregates modular files so users can (require 'test-flow-modular)
;; while we gradually migrate implementation from the monolith (test-flow.el).
;;
;; Hard requires: core facades (lightweight, safe).
;; Soft requires: optional UI/integration modules.

;;; Code:

;; Core facades (safe, lightweight)
(require 'test-flow-core)
(require 'test-flow-parse)
(require 'test-flow-runner)
(require 'test-flow-panel)
(require 'test-flow-render)
(require 'test-flow-watch)
(require 'test-flow-copy)

;; Optional UI/integrations (soft)
(require 'test-flow-headerline nil 'noerror)
(require 'test-flow-view-controls nil 'noerror)
(require 'test-flow-controls-icons nil 'noerror)
(require 'test-flow-coverage nil 'noerror)
(require 'test-flow-transient nil 'noerror)
(require 'test-flow-i18n nil 'noerror)
(require 'test-flow-spinner nil 'noerror)

(provide 'test-flow-modular)
;; Re-export the main feature to allow `(require 'test-flow-modular)` as a drop-in.
(provide 'test-flow)
;;; test-flow-modular.el ends here
