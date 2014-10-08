;;; init-flycheck.el --- Flycheck configuration
;;; Commentary:
;;;
;;; By jmgpena
;;;
;;; Code:

;; configure flycheck mode
(require-package 'flycheck)
(require 'flycheck)
(add-hook 'after-init-hook
          (lambda ()
            (global-flycheck-mode +1)
            (add-hook 'prog-mode-hook 'flycheck-mode)
            ))

;; Override default flycheck triggers
(setq flycheck-check-syntax-automatically '(save idle-change mode-enabled)
      flycheck-idle-change-delay 0.8)

(setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)

(provide 'init-flycheck)
;;; init-flycheck.el ends here
