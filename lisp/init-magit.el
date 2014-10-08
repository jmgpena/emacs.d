;;; init-magit.el
(require-package 'magit)

(global-set-key (kbd "C-x m") 'magit-status)
(autoload 'magit-status "magit")

(require-package 'git-timemachine)

(provide 'init-magit)
;;; ini-magit.el ends here
