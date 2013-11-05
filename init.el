;;; init.el --- emacs initialization
;;; commentary:
;;;

;;; code:

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

;; load split config files
(load "~/.emacs.d/gui")
(load "~/.emacs.d/basics")
(load "~/.emacs.d/prog")

;; Setup packages
(require 'init-package)

;; load el-get

;; local package list
(defun init--install-packages ()
  (packages-install
   '(magit
     org-plus-contrib
     ;;paredit
     ;;move-text
     ;;god-mode
     ;;gist
     htmlize
     visual-regexp
     flycheck
     flx
     flx-ido
     css-eldoc
     yasnippet
     smartparens
     ido-vertical-mode
     ido-at-point
     ;;simple-httpd
     guide-key
     nodejs-repl
     restclient
     highlight-escape-sequences
     whitespace-cleanup-mode
     elisp-slime-nav
     git-commit-mode
     gitconfig-mode
     gitignore-mode
     clojure-mode
     nrepl)))

(setq el-get-sources
              '(
                ;; basic packages
                (:name el-get)
                (:name org-plus-contrib
                       :type elpa
                       :repo ("org" . "http://orgmode.org/elpa/"))
                (:name ido-ubiquitous
                       :type elpa
                       :repo ("marmalade" . "http://marmalade-repo.org/packages/"))
                (:name flx)
                (:name ido-vertical-mode)
                (:name guide-key
                       type: github
                       pkgname "kbkbkbkb1/guide-key"
                       description "guide-key.el enables to guide the following
                                    key bindings automatically and dynamically")
                (:name rainbow-mode)
                ;; GUI packages
                (:name solarized-emacs
                       :type github
                       :pkgname "bbatsov/solarized-emacs"
                       :description "Solarized themes for Emacs"
                       :prepare (add-to-list 'custom-theme-load-path
                                             default-directory))
                (:name sr-speedbar)
                ;; programming
                (:name magit)
                (:name smartparens)
                (:name rainbow-delimiters)
                (:name flycheck)
                (:name helm)
                (:name projectile)
                (:name perspective)
                (:name php-mode)
                (:name web-mode)
                (:name sass-mode)
                (:name js2-mode)
                (:name jade-mode
                       :type github
                       :pkgname "brianc/jade-mode")
                )
              )

;; load and initialize packages
(el-get 'sync (mapcar 'el-get-source-name el-get-sources))

;; load the main config from the `after-init-hook' so all packages are loaded
(add-hook 'after-init-hook
 `(lambda ()
    ;; remember this directory
        ))

;;; init.el ends here
