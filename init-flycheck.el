;; configure flycheck mode
(add-hook 'after-init-hook
          (lambda ()
            (global-flycheck-mode +1)
            (add-hook 'prog-mode-hook 'flycheck-mode)
            ))
