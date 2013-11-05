;; configure flycheck mode
(require-package 'flycheck)
(require 'flycheck)
(add-hook 'after-init-hook
          (lambda ()
            (global-flycheck-mode +1)
            (add-hook 'prog-mode-hook 'flycheck-mode)
            ))

(provide 'init-flycheck)
