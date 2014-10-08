;;; init-php --- setup php lang settings
;;; Commentary:
;;; Code:
(require-package 'php-mode)
(require-package 'php-extras)
(require-package 'geben)

;; do not use php-mode on templates (with htlm)
(setq php-template-compatibility nil)

;; php interactive shell
(require-package 'inf-php)
(require 'inf-php)

(provide 'init-php)
;;; init-php ends here
