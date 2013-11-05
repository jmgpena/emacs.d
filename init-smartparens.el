;;; init-smartparens.el --- smartparens mode configuration
;;; Commentary:

;;; code:
(require-package 'smartparens)

(require 'smartparens-config)
(smartparens-global-mode t)
(show-smartparens-global-mode +1)
;; disable string quotes
(setq sp-autoescape-string-quote nil)

(provide 'init-smartparens)
;;; init-smartparens.el ends here
