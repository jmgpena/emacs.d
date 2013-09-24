;;; init.el --- emacs initialization
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


;; load system specific config settings
(setq config:sysinit-file
      (concat "~/.emacs.d/" (symbol-name system-type) ".el"))
(when (file-exists-p config:sysinit-file)
  (load config:sysinit-file))

;; load split config files
(load "~/.emacs.d/gui")
(load "~/.emacs.d/basics")

;; load el-get
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(setq el-get-user-package-directory "~/.emacs.d/")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))

;; local package list
(setq el-get-sources
      '(
        ;; basic packages
        (:name el-get)
        (:name org-plus-contrib
                         :type elpa
                         :repo ("org" . "http://orgmode.org/elpa/"))
        ;; GUI packages
        (:name solarized-emacs
               :type github
               :pkgname "bbatsov/solarized-emacs"
               :description "Solarized themes for Emacs"
               :prepare (add-to-list 'custom-theme-load-path
                                     default-directory))
        (:name sr-speedbar)
        ;; programming
        (:name smartparens)
        (:name flycheck)
        (:name helm)
        (:name projectile)
        (:name php-mode)
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
