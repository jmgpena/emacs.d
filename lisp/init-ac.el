;;; init-ac.el --- Initialize auto-complete package
;;; Commentary:
;;; hmmm
;;; Code:

(require-package 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)
(global-auto-complete-mode t)

;;; set the trigger key so that it can work together with yasnippet on tab key,
;;; if the word exists in yasnippet, pressing tab will cause yasnippet to
;;; activate, otherwise, auto-complete will
(ac-set-trigger-key "TAB")
(ac-set-trigger-key "<tab>")

(provide 'init-ac)
;;; init-ac.el ends here
