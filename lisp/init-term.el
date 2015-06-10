(use-package multi-term
  :ensure t
  :init
  (progn
    (setq multi-term-program "/bin/bash")
    (add-hook 'term-mode-hook (lambda () (setq show-trailing-whitespace nil)))))

(provide 'init-term)
