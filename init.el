;;; init.el --- emacs initialization
;;; Commentary:
;;; Code:

;; Show debugger on error
(setq debug-on-error t)

;; Setup proxy if it exists
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

;; Set path to elisp dependencies
(setq site-lisp-dir
      (expand-file-name "site-lisp" user-emacs-directory))
(add-to-list 'load-path user-emacs-directory)
(add-to-list 'load-path site-lisp-dir)

;; load system specific config settings
(defvar config:sysinit-file
      (concat "~/.emacs.d/" (symbol-name system-type) ".el"))
(when (file-exists-p config:sysinit-file)
  (load config:sysinit-file))

;; user configuration options
(setq user-full-name "Jorge Pena"
      user-mail-address "jorge@jmgpena.net")

;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; Add external projects to load path
(dolist (project (directory-files site-lisp-dir t "\\w+"))
  (when (file-directory-p project)
    (add-to-list 'load-path project)))

;; Load solarized theme
(require 'init-solarized-emacs)

;; load split config files
(load "~/.emacs.d/gui")
(load "~/.emacs.d/basics")
(load "~/.emacs.d/prog")

;; Setup packages
(require 'init-package)

;; load packages and configs
(require 'init-magit)
(require 'init-org-plus-contrib)
(require-package 'htmlize)
(require-package 'move-text)
(require 'init-flycheck)
(require 'init-ido)
(require 'init-yasnippet)
(require 'init-smartparens)
(require-package 'nodejs-repl)
(require-package 'highlight-escape-sequences)
(require-package 'perspective)
(persp-mode t)
(require 'init-projectile)
(require 'init-php)
(require 'init-web-mode)
(require 'init-multiple-cursors)
;; Javascript
(require-package 'js2-mode)
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jshintrc$" . javascript-mode))
(add-to-list 'magic-mode-alist '("#!/usr/bin/env node" . js2-mode))
(eval-after-load 'js2-mode '(require 'init-js2-mode))
(require-package 'coffee-mode)
(require-package 'json-mode)
(require 'init-skewer)
;; clojure
;(require 'init-clojure)
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
(require-package 'scss-mode)
(setq-default scss-compile-at-save nil)
(setq flycheck-scss-executable "scss --compass")
(setq css-indent-offset 2)
(eval-after-load 'flycheck '(setq flycheck-checkers (delq 'scss flycheck-checkers)))
;; ruby
(require-package 'rvm)
(rvm-use-default)
;;; init.el ends here
