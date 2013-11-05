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
     nrepl
     ido-ubiquitous
     rainbow-mode
     rainbow-delimiters
     projectile
     perspective
     php-mode
     web-mode
     sass-mode
     js2-mode
     )))


 ;;                      :prepare (add-to-list 'custom-theme-load-path
 ;;                                            default-

;;; init.el ends here
