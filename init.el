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

(require 'init-recentf)
(require 'init-ido)
(require 'init-hippie-expand)
(require 'init-auto-complete)
(require 'init-windows)
(require 'init-sessions)
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

;; rainbow delimiters
(require-package 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;;;; User and system configuration
(setq user-full-name "Jorge Pena"
      user-mail-address "jorge@jmgpena.net")

;;; Basic Settings

;; set the default directory
(setq default-directory "~/")
;; memory config - increase gc threshold to 20MB
(setq gc-cons-threshold 20000000)
;; locale settings and coding
(setq system-time-locale "C")
(prefer-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8-unix)
;; set environment coding system
(set-language-environment "UTF-8")
;; set TAB and indention
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
;; y or n is suffice for a yes or no question
(fset 'yes-or-no-p 'y-or-n-p)

;; set text-mode as the default major mode, instead of fundamental-mode
;; The first of the two lines in parentheses tells Emacs to turn on Text mode
;; when you find a file, unless that file should go into some other mode, such
;; as C mode.
(setq-default major-mode 'text-mode)


;; save backups to temporary dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))


;;; save last point in files
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (concat user-emacs-directory "saved-places"))

;; use aspell instead of ispell
(setq-default ispell-program-name "aspell"
              ispell-extra-args '("--sug-mode=ultra"))

; whitespace settings
(require 'whitespace)
(setq whitespace-style '(face spaces tabs newline space-mark tab-mark
                              newline-mark lines-tail trailing))
(setq whitespace-display-mappings
      '((space-mark 32 [183] [46])
        (newline-mark 10 [182 10])
        (tab-mark 9 [9655 9] [92 9])))
(setq whitespace-line-column 80)
;;; nice defaults
(setq-default apropos-do-all t
              mouse-yank-at-point t)

;;; scrolling behaviour
(setq redisplay-dont-pause t
      scroll-margin 1
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)
; mouse wheel scrolling
(setq mouse-wheel-follow-mouse 't)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))

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

;;;; Keybindings

;; automatically indent after return
(define-key global-map (kbd "RET") 'newline-and-indent)


;;;; Elisp
(setq lisp-indent-offset nil)
(defun imenu-elisp-sections ()
  "Setup imenu to look for comment section in elisp files."
  (setq imenu-prev-index-position-function nil)
  (add-to-list 'imenu-generic-expression '("Sections" "^;;;; \\(.+\\)$" 1) t))

(add-hook 'emacs-lisp-mode-hook 'imenu-elisp-sections)

(define-key emacs-lisp-mode-map (kbd "C-c C-z") 'ielm)

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (turn-on-eldoc-mode)
            (setq mode-name "EL")))


;;;; Load solarized theme
(require 'init-solarized-emacs)
;; smart mode line
(require-package 'smart-mode-line)
(sml/setup)
;; load packages and configs
(require-package 'editorconfig)
(require-package 'pos-tip)
(require 'init-yasnippet)

(require-package 'move-text)
(require 'init-smartparens)
(require-package 'highlight-escape-sequences)
(require-package 'perspective)
(persp-mode t)
(require 'init-projectile)

;;; color-identifiers-mode
(require-package 'color-identifiers-mode)
;; restclient
(require-package 'restclient)
(add-to-list 'auto-mode-alist '("\\.restclient$" . restclient-mode))
;; jade and stylus
(require-package 'jade-mode)
(require-package 'stylus-mode)
;; clojure
(require 'init-clojure)
;; Visual regexp
(require-package 'visual-regexp)
(define-key global-map (kbd "C-c r") 'vr/replace)
(define-key global-map (kbd "C-c q") 'vr/query-replace)
;; if you use multiple-cursors, this is for you:
(define-key global-map (kbd "C-c m") 'vr/mc-mark)
;; company (better autocompletion)
(require-package 'company)
(global-company-mode 1)
;; sr-speedbar
(require-package 'sr-speedbar)
;; sunrise commmander stuff
(require-package 'sunrise-commander)
(require-package 'sunrise-x-buttons)
;;; init.el ends here
