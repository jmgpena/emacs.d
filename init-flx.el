;; use flx for ido
(require 'ido)
(require 'ido-ubiquitous)
(require 'flx)
(require 'flx-ido)
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
