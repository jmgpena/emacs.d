;;; init.el --- emacs initialization
;;; Commentary:
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

;; System type constants
(defconst *is-a-mac* (eq system-type 'darwin))
(defconst *is-a-pc* (eq system-type 'windows-nt))
(defconst *is-a-linux* (eq system-type 'linux))

;; Keep emacs Custom-settings in separate file
(let ((custom-file-name (expand-file-name "custom.el" user-emacs-directory)))
  (setq custom-file custom-file-name)
  (when (file-exists-p custom-file-name)
    (load custom-file-name)))

;; add lisp dir to load path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; load system specific config settings
(defvar config:sysinit-file
      (concat "~/.emacs.d/lisp/" (symbol-name system-type) ".el"))
(when (file-exists-p config:sysinit-file)
  (load config:sysinit-file))

;;;----------------------------------------------------------------------------
;;; User and system settings
;;;----------------------------------------------------------------------------
(setq user-full-name "Jorge Pena"
      user-mail-address "jorge@jmgpena.net")
;; set the default directory
(setq default-directory "~/")
;; memory config - increase gc threshold to 20MB
(setq gc-cons-threshold 20000000)
;; save backups to temporary dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;;----------------------------------------------------------------------------
;; Bootstrap config
;;-----------------------------------------------------------------------------
(require 'init-utils)
(require 'init-site-lisp)
(require 'init-elpa)

;;-----------------------------------------------------------------------------
;; Load configs for specific features and modes
;;-----------------------------------------------------------------------------
(require-package 'wgrep) ;; edit grep/ag files in results buffer

(require 'init-frame-hooks)
(require 'init-xterm)
(require 'init-themes)
(require 'init-osx-keys)
(require 'init-gui-frames)
(require 'init-dired)
(require 'init-isearch)
(require 'init-grep)
(require 'init-uniquify)
(require 'init-ibuffer)
(require 'init-flycheck)
(require 'init-smartparens)

(require 'init-recentf)
;(require 'init-ido)
(require 'init-helm)
(require 'init-hippie-expand)
;;(require 'init-auto-complete)
(require 'init-windows)
;;(require 'init-sessions)
(require 'init-fonts)

(require 'init-editing-utils)

(require 'init-vc)
(require 'init-markdown)
(require 'init-csv)
(require 'init-javascript)
(require 'init-php)
(require 'init-org)
(require 'init-nxml)
(require 'init-html)
(require 'init-css)
(require 'init-haskell)
(require 'init-ruby)
(require 'init-sql)

(require 'init-lisp)
(require 'init-clojure)
(require 'init-locales)
(require 'init-shell)
(require 'init-spelling)
(require 'init-yasnippet)
(require 'init-projectile)
(require 'init-speedbar)
(require 'init-elfeed)

;;;----------------------------------------------------------------------------
;;; Packages without configuration
;;;----------------------------------------------------------------------------
;; rainbow delimiters
(use-package rainbow-delimiters :ensure t)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
;; smart mode line
(require-package 'smart-mode-line)
(sml/setup)
;; load packages and configs
(require-package 'editorconfig)
(require-package 'pos-tip)
(require-package 'move-text)
(require-package 'highlight-escape-sequences)
;;; color-identifiers-mode
(require-package 'color-identifiers-mode)
;; restclient
(require-package 'restclient)
(add-to-list 'auto-mode-alist '("\\.restclient$" . restclient-mode))
;; jade and stylus
(require-package 'jade-mode)
(require-package 'stylus-mode)
;; Visual regexp
(require-package 'visual-regexp)
(define-key global-map (kbd "C-c r") 'vr/replace)
(define-key global-map (kbd "C-c q") 'vr/query-replace)
;; if you use multiple-cursors, this is for you:
(define-key global-map (kbd "C-c m") 'vr/mc-mark)
;; company (better autocompletion)
(require-package 'company)
(add-hook 'after-init-hook 'global-company-mode)
;; sunrise commmander stuff
(require-package 'sunrise-commander)
(require-package 'sunrise-x-buttons)
;; hungry-delete
(require-package 'hungry-delete)
(global-hungry-delete-mode)
;; karma runner support
(require-package 'karma)
;; org-journal
(use-package org-journal
  :ensure t
  :init
  (setq org-journal-dir "~/Dropbox/org/journal"))

(provide 'init)
;;; init.el ends here
