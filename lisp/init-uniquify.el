;;; init-uniquify.el --- Uniquify buffer names
;;; Commentary:
;;;
;;; By jmgpena
;;;
;;; Code:

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquify-separator " • ")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

(provide 'init-uniquify)
;;; init-uniquify.el ends here
