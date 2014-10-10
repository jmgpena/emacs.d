;;; init-css.el --- CSS configs
;;; Commentary:
;;;
;;; By jmgpena
;;;
;;; Code:

;; css
(require-package 'rainbow-mode)

;; settings
(setq css-indent-offset 4)

;; sass
(require-package 'scss-mode)
(require-package 'sass-mode)
(setq-default scss-compile-at-save nil)
(setq flycheck-scss-executable "scss -c")
(eval-after-load 'flycheck '(setq flycheck-checkers (delq 'scss flycheck-checkers)))
;; less
(require-package 'less-css-mode)
(when (featurep 'js2-mode)
  (require-package 'skewer-less))

;;; Use eldoc for syntax hints
(require-package 'css-eldoc)
(autoload 'turn-on-css-eldoc "css-eldoc")
(add-hook 'css-mode-hook 'turn-on-css-eldoc)

;;(rainbow-mode +1)

(provide 'init-css)
;;; init-css.el ends here
