;;; init-magit.el
(require-package 'magit)

(global-set-key (kbd "C-x m") 'magit-status)
(autoload 'magit-status "magit")

(provide 'init-magit)
;;; ini-magit.el ends here
