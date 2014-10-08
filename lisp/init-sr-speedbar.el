;;; init-sr-speedbar.el --- speedbar config
;;; commentary:

;;; code:
(require 'sr-speedbar)

(setq speedbar-hide-button-brackets-flag nil
      speedbar-show-unknown-files t
      speedbar-smart-directory-expand-flag t
      speedbar-directory-button-trim-method 'trim
      speedbar-use-images t
      speedbar-indentation-width 2
      speedbar-use-imenu-flag t
      speedbar-file-unshown-regexp "flycheck-.*"
      sr-speedbar-width 40
      sr-speedbar-width-x 40
      sr-speedbar-auto-refresh nil
      sr-speedbar-skip-other-window-p t
      sr-speedbar-right-side nil)

;; keybindings
(global-set-key (kbd "s-t") 'sr-speedbar-toggle)
(define-key speedbar-mode-map [S-up] 'speedbar-up-directory)
(define-key speedbar-mode-map [right] 'speedbar-flush-expand-line)
(define-key speedbar-mode-map [left] 'speedbar-contract-line)

;;; init-sr-speedbar ends here
