;;; test-flow-i18n.el --- i18n for test-flow (labels/messages) -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Centralized localization for test-flow components (transient, messages).
;; - defcustom `test-flow-language' ('auto | 'ru | 'en)
;; - tables for RU/EN
;; - function `test-flow-i18n' to fetch a localized string by key

;;; Code:

(eval-when-compile (require 'subr-x))

;;;###autoload
(defgroup test-flow-i18n nil
  "Localization support for test-flow."
  :group 'test-flow)

;;;###autoload
(defcustom test-flow-language 'auto
  "Language for labels/messages: 'ru, 'en, or 'auto."
  :type '(choice (const :tag "Auto" auto)
                 (const :tag "English" en)
                 (const :tag "Russian" ru))
  :group 'test-flow-i18n)

(defun test-flow-i18n--lang-ru-p ()
  (pcase test-flow-language
    ('ru t)
    ('en nil)
    (_ (let* ((env current-language-environment)
              (name (and (stringp env) env)))
         (or (string-match-p "\\bRussian\\b" (or name ""))
             (string-match-p "\\bru\\b" (or (getenv "LANG") "")))))))

(defconst test-flow-i18n--en
  '((title . "Test Flow")
    (sec-run . "Run")
    (sec-coverage . "Coverage")
    (sec-report . "Reports")
    (sec-ui . "UI")
    (run-all . "Run all")
    (run-file . "Run file")
    (run-at-point . "Run test at point")
    (rerun . "Re-run last")
    (cov-toggle . "Toggle coverage")
    (cov-show . "Show coverage")
    (cov-refresh . "Refresh coverage")
    (report-open . "Open report")
    (log-open . "Open last log")
    (report-copy . "Copy last report path")
    (panel-toggle . "Toggle panel")
    (headerline-toggle . "Toggle headerline")
    (transient-missing . "Package 'transient' is not available. Please install it.")
    (no-command . "No suitable command is available in this environment.")
    (bound-conflict . "Key %s is already globally bound to %s; not overriding (set test-flow-global-transient-override-existing to t to force).")
    (bound-applied . "Key %s bound to %s.")
    (key-updated . "Key updated: %s -> %s")
    (runner-detect . "Detect test runner")
    (show-progress . "Show live progress")
    (status-toggle . "Toggle status split")
    (invalid-key . "Invalid key: %S"))
  "English strings.")

(defconst test-flow-i18n--ru
  '((title . "Test Flow")
    (sec-run . "Запуск")
    (sec-coverage . "Покрытие")
    (sec-report . "Отчёты")
    (sec-ui . "Интерфейс")
    (run-all . "Запустить все")
    (run-file . "Запустить файл")
    (run-at-point . "Запустить тест под курсором")
    (rerun . "Повторить последний")
    (cov-toggle . "Переключить покрытие")
    (cov-show . "Показать покрытие")
    (cov-refresh . "Обновить покрытие")
    (report-open . "Открыть отчёт")
    (log-open . "Открыть последний лог")
    (report-copy . "Скопировать путь последнего отчёта")
    (panel-toggle . "Переключить панель")
    (headerline-toggle . "Переключить заголовок буфера")
    (transient-missing . "Пакет 'transient' недоступен. Установите его.")
    (no-command . "Подходящая команда в этом окружении недоступна.")
    (bound-conflict . "Клавиша %s уже глобально привязана к %s; не переопределяем (установите test-flow-global-transient-override-existing в t для принудительного замещения).")
    (bound-applied . "Клавиша %s привязана к %s.")
    (key-updated . "Клавиша обновлена: %s -> %s")
    (runner-detect . "Определить тест-раннер")
    (show-progress . "Показать прогресс выполнения")
    (status-toggle . "Переключить окно статуса")
    (invalid-key . "Некорректная клавиша: %S"))
  "Russian strings.")

;;;###autoload
(defun test-flow-i18n (key)
  "Return localized string for KEY."
  (let* ((ru? (test-flow-i18n--lang-ru-p))
         (tbl (if ru? test-flow-i18n--ru test-flow-i18n--en)))
    (or (alist-get key tbl) (format "%s" key))))

(provide 'test-flow-i18n)
;;; test-flow-i18n.el ends here
