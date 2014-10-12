;;; init-locales.el --- Locales config
;;; Commentary:
;;;
;;; By jmgpena
;;;
;;; Code:

;; locale settings and coding
(setq system-time-locale "C")
(setq locale-coding-system 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8-unix)
(set-selection-coding-system 'utf-8-unix)
;; set environment coding system
(set-language-environment "UTF-8")

(provide 'init-locales)
;;; init-locales.el ends here
