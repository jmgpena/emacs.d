;;; init-ido.el --- Ido configuration
;;; Commentary:
;;;
;;; By jmgpena
;;;
;;; Code:

;; use flx for ido
(require 'ido)
(require-package 'ido-ubiquitous)
(require-package 'flx)
(require-package 'flx-ido)

;; ido settings
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ;ido-use-filename-at-point 'guess
      ido-max-prospects 10
      ido-default-file-method 'selected-window
      ido-auto-merge-work-directories-lenght -1
      ido-save-directory-list-file "~/.emacs.d/.ido.last"
      ido-use-virtual-buffers t
      ido-handle-duplicate-virtual-buffers 2)
(ido-mode +1)
(ido-ubiquitous-mode +1)
(flx-ido-mode +1)
;; disable faces to see flx highlights
(setq ido-use-faces nil)

;; use vertical mode
(require-package 'ido-vertical-mode)
(ido-vertical-mode)

;; Ido at point (C-,)
(require-package 'ido-at-point)
(ido-at-point-mode)

;; Fix ido-ubiquitous for newer packages
(defmacro ido-ubiquitous-use-new-completing-read (cmd package)
  `(eval-after-load ,package
     '(defadvice ,cmd (around ido-ubiquitous-new activate)
        (let ((ido-ubiquitous-enable-compatibility nil))
          ad-do-it))))

(ido-ubiquitous-use-new-completing-read webjump 'webjump)
(ido-ubiquitous-use-new-completing-read yas-expand 'yasnippet)
(ido-ubiquitous-use-new-completing-read yas-visit-snippet-file 'yasnippet)

;; use smex for M-x
(require-package 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
;; Use C-x C-m to do M-x per Steve Yegge's advice
(global-set-key (kbd "C-x C-m") 'smex)

(require-package 'idomenu)
;; imenu
(set-default 'imenu-auto-rescan t)
(global-set-key (kbd "C-x C-i") 'idomenu)


;; get rid of `find-file-read-only' and replace it with something
;; more useful.
(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file."
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))
(global-set-key (kbd "C-x C-r") 'ido-recentf-open)

(provide 'init-ido)
;;; init-ido.el ends here
