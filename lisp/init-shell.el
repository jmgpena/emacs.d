;;; init-shell.el --- Shell config
;;; Commentary:
;;;
;;; By jmgpena
;;;
;;; Code:

;; shell-mode settings
(unless *is-a-pc*
  (setq-default explicit-shell-file-name "/bin/bash")
  (setq-default shell-file-name "/bin/bash"))
;; always insert at the bottom
(setq-default comint-scroll-to-bottom-on-input t)
;; no duplicates in command history
(setq-default comint-input-ignoredups t)
;; what to run when i press enter on a line above the current prompt
(setq-default comint-get-old-input (lambda () ""))
;; max shell history size
(setq-default comint-input-ring-size 1000)
;; show all in emacs interactive output
(setenv "PAGER" "cat")
;; set lang to enable Chinese display in shell-mode
(setenv "LANG" "en_US.UTF-8")

(provide 'init-shell)
;;; init-shell.el ends here
