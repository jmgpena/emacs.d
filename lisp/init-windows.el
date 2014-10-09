;;; init-windows.el --- Gui Windows config
;;; Commentary:
;;;
;;; By jmgpena
;;;
;;; Code:

;;;; Move between windows
;; navigate windows with M-<arrows>
(windmove-default-keybindings 'meta)
(setq windmove-wrap-around t)
;; ace-window package replace the default emacs other-window keybinding because
;; the behavior is the same for 2 windows and is nice to get ace-window for
;; more thant 2
(require-package 'ace-window)
(global-set-key (kbd "C-x o") 'ace-window)
;;(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)) ; use letters instead of numbers

;;----------------------------------------------------------------------------
;; When splitting window, show (other-buffer) in the new window
;;----------------------------------------------------------------------------
(defun split-window-func-with-other-buffer (split-function)
  (lexical-let ((s-f split-function))
    (lambda ()
      (interactive)
      (funcall s-f)
      (set-window-buffer (next-window) (other-buffer)))))

(global-set-key "\C-x2" (split-window-func-with-other-buffer 'split-window-vertically))
(global-set-key "\C-x3" (split-window-func-with-other-buffer 'split-window-horizontally))

(defun site/toggle-delete-other-windows ()
  "Delete other windows in frame if any, or restore previous window config."
  (interactive)
  (if (and winner-mode
           (equal (selected-window) (next-window)))
      (winner-undo)
    (delete-other-windows)))

(global-set-key "\C-x1" 'site/toggle-delete-other-windows)

;;----------------------------------------------------------------------------
;; Rearrange split windows
;;----------------------------------------------------------------------------
(defun split-window-horizontally-instead ()
  (interactive)
  (save-excursion
    (delete-other-windows)
    (funcall (split-window-func-with-other-buffer 'split-window-horizontally))))

(defun split-window-vertically-instead ()
  (interactive)
  (save-excursion
    (delete-other-windows)
    (funcall (split-window-func-with-other-buffer 'split-window-vertically))))

(global-set-key "\C-x|" 'split-window-horizontally-instead)
(global-set-key "\C-x_" 'split-window-vertically-instead)

;;----------------------------------------------------------------------------
;; Rearrange split windows
;;----------------------------------------------------------------------------
(defun split-window-horizontally-instead ()
  (interactive)
  (save-excursion
    (delete-other-windows)
    (funcall (split-window-func-with-other-buffer 'split-window-horizontally))))

(defun split-window-vertically-instead ()
  (interactive)
  (save-excursion
    (delete-other-windows)
    (funcall (split-window-func-with-other-buffer 'split-window-vertically))))

(global-set-key "\C-x|" 'split-window-horizontally-instead)
(global-set-key "\C-x_" 'split-window-vertically-instead)

;; Borrowed from http://postmomentum.ch/blog/201304/blog-on-emacs
(defun site/split-window()
  "Split the window to see the most recent buffer in the other window.
Call a second time to restore the original window configuration."
  (interactive)
  (if (eq last-command 'site/split-window)
      (progn
        (jump-to-register :site/split-window)
        (setq this-command 'site/unsplit-window))
    (window-configuration-to-register :site/split-window)
    (switch-to-buffer-other-window nil)))

(global-set-key (kbd "<f7>") 'site/split-window)
(global-set-key (kbd "<f6>")
                (lambda ()
                  (interactive)
                  (switch-to-buffer nil)))

(provide 'init-windows)
;;; init-windows.el ends here
