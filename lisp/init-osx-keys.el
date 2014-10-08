;;; init-osx-keys.el --- OSX key config
;;; Commentary:
;;;
;;; By jmgpena
;;;
;;; Code:

(when *is-a-mac*
  ;; mac keys
  (setq mac-control-modifier 'ctrl)
  (setq mac-option-modifier 'super)
  (setq mac-command-modifier 'meta)
  (setq mac-right-option-modifier 'none)
  (setq ns-function-modifier 'hyper)
  (setq default-input-method "MacOSX")
  ;; antialising
  (setq mac-allow-anti-aliasing t)
  ;; Make mouse wheel / trackpad scrolling less jerky
  (setq mouse-wheel-scroll-amount '(1
                                    ((shift) . 5)
                                    ((control))))
  (dolist (multiple '("" "double-" "triple-"))
    (dolist (direction '("right" "left"))
      (global-set-key (kbd (concat "<" multiple "wheel-" direction ">")) 'ignore)))
  (global-set-key (kbd "M-`") 'ns-next-frame)
  (global-set-key (kbd "M-h") 'ns-do-hide-emacs)
  (global-set-key (kbd "M-˙") 'ns-do-hide-others)
  (after-load 'nxml-mode
    (define-key nxml-mode-map (kbd "M-h") nil))
  (global-set-key (kbd "M-_") 'ns-do-hide-others) ;; what describe-key reports for cmd-option-h
  )

(provide 'init-osx-keys)
;;; init-osx-keys.el ends here
