;;; init.el --- emacs initialization
;;; Commentary:
;;;
;;; This file loads the main configuration file in org format
;;;
;;; By jmgpena
;;;
;;; Code:

;; Show debugger on error
(setq debug-on-error t)
;; This only works for emacs 24 and up
(let ((minver 24))
  (unless (>= emacs-major-version minver)
    (error "Your Emacs is too old -- this config requries v%s or higher!" minver)))

;; locale settings and coding
(setq system-time-locale "C")
(setq locale-coding-system 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8-unix)
(set-selection-coding-system 'utf-8-unix)
;; set environment coding system
(set-language-environment "UTF-8")

(org-babel-load-file "~/.emacs.d/jmgpena.org")

;; System type constants
;; (defconst *is-a-mac* (eq system-type 'darwin))
;; (defconst *is-a-pc* (eq system-type 'windows-nt))
;; (defconst *is-a-linux* (eq system-type 'linux))

;; load system specific config settings
;; (defvar config:sysinit-file
;;   (concat "~/.emacs.d/lisp/" (symbol-name system-type) ".el"))
;; (when (file-exists-p config:sysinit-file)
;;   (load config:sysinit-file))

;;----------------------------------------------------------------------------
;; Bootstrap config
;;-----------------------------------------------------------------------------
;; (require 'init-utils)
;; (require 'init-site-lisp)
;; (require 'init-elpa)

;;-----------------------------------------------------------------------------
;; Load configs for specific features and modes
;;-----------------------------------------------------------------------------
;; (require-package 'wgrep) ;; edit grep/ag files in results buffer

;; (require 'init-frame-hooks)
;; (require 'init-xterm)
;; (require 'init-themes)
;; (require 'init-osx-keys)
;; (require 'init-gui-frames)
;; (require 'init-dired)
;; (require 'init-isearch)
;; (require 'init-grep)
;; (require 'init-uniquify)
;; (require 'init-ibuffer)
;; (require 'init-flycheck)
;; (require 'init-smartparens)

;; (require 'init-recentf)
;; ;(require 'init-ido)
;; (require 'init-helm)
;; (require 'init-hippie-expand)
;; ;;(require 'init-auto-complete)
;; (require 'init-windows)
;; ;;(require 'init-sessions)
;; (require 'init-fonts)

;; (require 'init-editing-utils)

;; (require 'init-markdown)
;; (require 'init-csv)
;; (require 'init-javascript)
;; (require 'init-php)
;; (require 'init-org)
;; (require 'init-nxml)
;; (require 'init-html)
;; (require 'init-css)
;; (require 'init-haskell)
;; (require 'init-ruby)
;; (require 'init-sql)

;; (require 'init-lisp)
;; (require 'init-clojure)
;; (require 'init-locales)
;; (require 'init-shell)
;; (require 'init-spelling)
;; (require 'init-yasnippet)
;; (require 'init-projectile)
;; (require 'init-speedbar)
;; (require 'init-elfeed)
;; (require 'init-hydra)

;; ;;;----------------------------------------------------------------------------
;; ;;; Packages without configuration
;; ;;;----------------------------------------------------------------------------
;; ;; load packages and configs
;; (require-package 'editorconfig)
;; (require-package 'pos-tip)
;; (require-package 'move-text)
;; (require-package 'highlight-escape-sequences)
;; ;;; color-identifiers-mode
;; (require-package 'color-identifiers-mode)
;; (use-package rainbow-identifiers :ensure t)
;; ;; restclient
;; (require-package 'restclient)
;; (add-to-list 'auto-mode-alist '("\\.restclient$" . restclient-mode))
;; ;; lua
;; (use-package lua-mode :ensure t)
;; ;; jade and stylus
;; (require-package 'jade-mode)
;; (require-package 'stylus-mode)
;; ;; Visual regexp
;; (require-package 'visual-regexp)
;; (define-key global-map (kbd "C-c r") 'vr/replace)
;; (define-key global-map (kbd "C-c q") 'vr/query-replace)
;; ;; if you use multiple-cursors, this is for you:
;; (define-key global-map (kbd "C-c m") 'vr/mc-mark)
;; ;; company (better autocompletion)
;; (require-package 'company)
;; (add-hook 'after-init-hook 'global-company-mode)
;; ;; sunrise commmander stuff
;; (require-package 'sunrise-commander)
;; (require-package 'sunrise-x-buttons)
;; ;; hungry-delete
;; (require-package 'hungry-delete)
;; (global-hungry-delete-mode)
;; ;; karma runner support
;; (require-package 'karma)
;; ;; org-journal
;; (use-package org-journal
;;   :ensure t
;;   :init
;;   (setq org-journal-dir "~/Dropbox/org/journal"))
;; ;; vagrant
;; (use-package vagrant :ensure t)
;; ;; ledger-mode
;; (use-package ledger-mode
;;   :ensure t
;;   :mode "\\.ledger\\'"
;;   :init
;;   (setq ledger-post-auto-adjust-amounts t))
;; (use-package sx :ensure t)
;; (use-package todotxt
;;   :ensure t
;;   :init
;;   (progn
;;     (setq todotxt-file "~/Dropbox/org/todo.txt")
;;     (global-set-key (kbd "C-x t") 'todotxt)))

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(paradox-github-token t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
