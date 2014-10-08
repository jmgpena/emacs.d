;;; init-gui-frames.el --- Gui frames stuff
;;; Commentary:
;;;
;;; By jmgpena
;;;
;;; Code:

;;----------------------------------------------------------------------------
;; Stop C-z from minimizing windows under OS X
;;----------------------------------------------------------------------------
(defun maybe-suspend-frame ()
  (interactive)
  (unless (and *is-a-mac* window-system)
    (suspend-frame)))

(global-set-key (kbd "C-z") 'maybe-suspend-frame)

;;----------------------------------------------------------------------------
;; Suppress GUI features
;;----------------------------------------------------------------------------
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)

;;----------------------------------------------------------------------------
;; Show a marker in the left fringe for lines not in the buffer
;;----------------------------------------------------------------------------
(setq indicate-empty-lines t)

;;----------------------------------------------------------------------------
;; Window size and features
;;----------------------------------------------------------------------------
(dolist (mode '( tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

(let ((no-border '(internal-border-width . 0)))
  (add-to-list 'default-frame-alist no-border)
  (add-to-list 'initial-frame-alist no-border))

(defun site/adjust-opacity (frame incr)
  "Adjust FRAME opacity by INCR."
  (let* ((oldalpha (or (frame-parameter frame 'alpha) 100))
         (newalpha (+ incr oldalpha)))
    (when (and (<= frame-alpha-lower-limit newalpha) (>= 100 newalpha))
      (modify-frame-parameters frame (list (cons 'alpha newalpha))))))

(when (and *is-a-mac* (fboundp 'toggle-frame-fullscreen))
  ;; Command-Option-f to toggle fullscreen mode
  ;; Hint: Customize `ns-use-native-fullscreen'
  (global-set-key (kbd "M-f") 'toggle-frame-fullscreen))

(global-set-key (kbd "M-C-8") '(lambda () (interactive) (site/adjust-opacity nil -5)))
(global-set-key (kbd "M-C-9") '(lambda () (interactive) (site/adjust-opacity nil 5)))
(global-set-key (kbd "M-C-0") '(lambda () (interactive) (modify-frame-parameters nil `((alpha . 100)))))

(add-hook 'after-make-frame-functions
          (lambda (frame)
            (with-selected-frame frame
              (unless window-system
                (set-frame-parameter nil 'menu-bar-lines 0)))))

(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; Non-zero values for `line-spacing' can mess up ansi-term and co,
;; so we zero it explicitly in those cases.
(add-hook 'term-mode-hook
          (lambda ()
            (setq line-spacing 0)))

(provide 'init-gui-frames)
;;; init-gui-frames.el ends here
