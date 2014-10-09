;;; init-php.el --- setup php lang settings
;;; Commentary:
;;; Code:
(require-package 'php-mode)
(require-package 'php-extras)
(require-package 'smarty-mode)
(require-package 'geben)

;; do not use php-mode on templates (with htlm)
(setq php-template-compatibility nil)

(provide 'init-php)
;;; init-php.el ends here
