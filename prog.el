;;; prog.el --- programming modes configuration
;;; commentary:
;;; programming defaults (mainly Emacs Lisp and internal stuff)

;;; code:
(define-key emacs-lisp-mode-map (kbd "C-c C-z") 'ielm)
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (turn-on-eldoc-mode)
            (setq mode-name "EL")

            ))
;;; prog.el ends here
