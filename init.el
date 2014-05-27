;;; init.el --- emacs initialization
;;; Commentary:
;;;
;;; By jmgpena
;;;
;;; Code:

;; Show debugger on error
(setq debug-on-error t)

;;;; User and system configuration
(setq user-full-name "Jorge Pena"
      user-mail-address "jorge@jmgpena.net")

;; load system specific config settings
(defvar config:sysinit-file
      (concat "~/.emacs.d/" (symbol-name system-type) ".el"))
(when (file-exists-p config:sysinit-file)
  (load config:sysinit-file))
;; frame font
(if (and (boundp 'config:font-family)
         (display-graphic-p))
    (set-face-attribute
     'default nil :font (concat config:font-family "-" config:font-size)))

;;; Basic Settings

;; remove toolbar and scrollbar
(dolist (mode '( tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))
;; Don't show splash screen
(setq inhibit-startup-message t)
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
;; auto revert buffer globally
(global-auto-revert-mode t)
;; set TAB and indention
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
;; y or n is suffice for a yes or no question
(fset 'yes-or-no-p 'y-or-n-p)
;; always add new line to the end of a file
(setq require-final-newline t)
;; add no new lines when "arrow-down key" at the end of a buffer
(setq next-line-add-newlines nil)
;; prevent the annoying beep on errors
(setq ring-bell-function 'ignore)
;; remove trailing whitespaces before save
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; enable to support navigate in camelCase words
(global-subword-mode t)
;; delete the selection with a keypress
(delete-selection-mode t)
;; show column number and line number
(dolist (mode '(column-number-mode line-number-mode))
  (when (fboundp mode) (funcall mode t)))
;; make the fringe thinner (default is 8 in pixels)
(fringe-mode 4)
;; Toggle line highlighting in all buffers
(global-hl-line-mode t)
;; Highlight tabs
(setq-default highlight-tabs t)
;; And trailing whitespace
(setq-default show-trailing-whitespace t)


;; set text-mode as the default major mode, instead of fundamental-mode
;; The first of the two lines in parentheses tells Emacs to turn on Text mode
;; when you find a file, unless that file should go into some other mode, such
;; as C mode.
(setq-default major-mode 'text-mode)

;; set fill-column for text mode
(setq-default fill-column 79)
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; save backups to temporary dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))


;;; save last point in files
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (concat user-emacs-directory "saved-places"))

;;; recent files
(require 'recentf)
;; save the .recentf file to .emacs.d/
(setq recentf-save-file (concat user-emacs-directory ".recentf"))
;; enable recent files mode.
(recentf-mode t)
;; 50 files ought to be enough.
(setq recentf-max-saved-items 50)
(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file."
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; use aspell instead of ispell
(setq-default ispell-program-name "aspell"
              ispell-extra-args '("--sug-mode=ultra"))

;; jump dired
(require 'dired-x)

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
(unless (eq system-type 'windows-nt)
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

;;;; Proxy configuration
(require 'url)
(let ((proxy (url-generic-parse-url (getenv "HTTP_PROXY"))))
  (when (stringp (url-host proxy))
    (setq url-proxy-services
          `(("http" . ,(concat (url-host proxy)
                               ":"
                               (number-to-string (url-port proxy))))
            ("https" . ,(concat (url-host proxy)
                                ":"
                                (number-to-string (url-port proxy))))
            ("ftp" . ,(concat (url-host proxy)
                              ":"
                              (number-to-string (url-port proxy))))))))

;;;; Paths

;; Set path to elisp dependencies
(defvar site-lisp-dir
    (expand-file-name "site-lisp" user-emacs-directory))
(add-to-list 'load-path user-emacs-directory)
(add-to-list 'load-path site-lisp-dir)

;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; Add external projects to load path
(dolist (project (directory-files site-lisp-dir t "\\w+"))
  (when (file-directory-p project)
    (add-to-list 'load-path project)))

;;;; Packages

(require 'package)
;; Define package archives
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t)
;;(add-to-list 'package-archives
;;             '("marmalade" . "http://marmalade-repo.org/packages/") t)

;; initialize package system
(package-initialize)
(unless
    (and
     (file-exists-p "~/.emacs.d/elpa/archives/melpa")
     (file-exists-p "~/.emacs.d/elpa/archives/org")
     (file-exists-p "~/.emacs.d/elpa/archives/marmalade"))
  (package-refresh-contents))

;; install dash if not already installed
(when (not (package-installed-p 'dash))
  (package-install 'dash))
(require 'dash)

(defun packages-install (packages)
    "Install packages from a list (PACKAGES)."
  (--each packages
    (when (not (package-installed-p it))
      (package-install it)))
  (delete-other-windows))

;;; On-demand installation of packages
(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (package-install package)
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))

;;;; Move between windows
;; navigate windows with M-<arrows>
(windmove-default-keybindings 'meta)
(setq windmove-wrap-around t)
;; ace-window package replace the default emacs other-window keybinding because
;; the behavior is the same for 2 windows and is nice to get ace-window for
;; more thant 2
(require-package 'ace-window)
(global-set-key (kbd "C-x o") 'ace-window)

;;;; Keybindings

;; automatically indent after return
(define-key global-map (kbd "RET") 'newline-and-indent)

;; get rid of `find-file-read-only' and replace it with something
;; more useful.
(global-set-key (kbd "C-x C-r") 'ido-recentf-open)

;; smart move to beginning of line
(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'smarter-move-beginning-of-line)

;;; global keyboard changes (from better defaults package)
;;; and other sources
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; don't quit that easily
(global-set-key (kbd "C-x r q") 'save-buffers-kill-terminal)
(global-set-key (kbd "C-x C-c") 'delete-frame)

;; imenu
(global-set-key (kbd "C-x C-i") 'imenu)


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

;; load packages and configs
(require-package 'editorconfig)
(require-package 'pos-tip)
(require 'init-yasnippet)
(require 'init-ac)
(require 'init-magit)
(require 'init-org-plus-contrib)
(require-package 'htmlize)
(require-package 'move-text)
(require 'init-flycheck)
(require 'init-ido)
(require 'init-smartparens)
(require-package 'highlight-escape-sequences)
(require-package 'perspective)
(persp-mode t)
(require 'init-projectile)
(require 'init-php)
(require 'init-web-mode)
(require 'init-multiple-cursors)
;;; color-identifiers-mode
(require-package 'color-identifiers-mode)
;; Javascript
(require-package 'js2-mode)
(require-package 'js2-refactor)
(require-package 'json-mode)
(require-package 'json-reformat)
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jshintrc$" . json-mode))
(add-to-list 'magic-mode-alist '("#!/usr/bin/env node" . js2-mode))
(eval-after-load 'js2-mode '(require 'init-js2-mode))
(require-package 'coffee-mode)
(require 'init-skewer)
(require-package 'nodejs-repl)
;; restclient
(require-package 'restclient)
(add-to-list 'auto-mode-alist '("\\.restclient$" . restclient-mode))
;; guide-key
(require-package 'guide-key)
(setq guide-key/guide-key-sequence '("C-x r" "C-x 4" "C-x v" "C-x 8" "C-x x"
                                     "C-c p"))
(guide-key-mode 1)
;; jade and stylus
(require-package 'jade-mode)
(require-package 'stylus-mode)
;; sass
(require-package 'rainbow-mode)
(rainbow-mode +1)
(require-package 'scss-mode)
(setq-default scss-compile-at-save nil)
(setq flycheck-scss-executable "scss -c")
(setq css-indent-offset 4)
(eval-after-load 'flycheck '(setq flycheck-checkers (delq 'scss flycheck-checkers)))
;; ruby
(require-package 'rvm)
(rvm-use-default)
;; ecb
(require-package 'ecb)
;; clojure
(require 'init-clojure)
;; Visual regexp
(require-package 'visual-regexp)
(define-key global-map (kbd "C-c r") 'vr/replace)
(define-key global-map (kbd "C-c q") 'vr/query-replace)
;; if you use multiple-cursors, this is for you:
(define-key global-map (kbd "C-c m") 'vr/mc-mark)
;; expand region
(require-package 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)
;;; init.el ends here
