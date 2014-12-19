;;; init-elpa.el --- package stuff
;;; Commentary:
;;;
;;; By jmgpena
;;;
;;; Code:

;; Proxy configuration
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

;; use package.el from emacs
(require 'package)

;; Define package archives
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives
             '("SC" . "http://joseito.republika.pl/sunrise-commander/"))
(add-to-list 'package-archives
             '("marmalade" . "https://marmalade-repo.org/packages/") t)

;; If gpg cannot be found, signature checking will fail, so we
;; conditionally enable it according to whether gpg is available. We
;; re-run this check once $PATH has been configured
(defun site/package-maybe-enable-signatures ()
  "Enable package signatures if gpg is present."
  (setq package-check-signature (when (executable-find "gpg") 'allow-unsigned)))

(site/package-maybe-enable-signatures)
(after-load 'init-exec-path
  (site/package-maybe-enable-signatures))

;; On-demand installation of packages
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

;; initialize package system
(package-initialize)

(unless
    (and
     (file-exists-p "~/.emacs.d/elpa/archives/melpa")
     (file-exists-p "~/.emacs.d/elpa/archives/org")
     (file-exists-p "~/.emacs.d/elpa/archives/SC")
     (file-exists-p "~/.emacs.d/elpa/archives/marmalade"))
  (package-refresh-contents))

;; load use-package from melpa
(require-package 'use-package)
(require 'use-package)

;; paradox package list replacement
(require-package 'paradox)
;; (setq paradox-github-token "08ceb37da5d8e34cfa6d2f74619d7a54fa0e1763"
;;       paradox-column-width-package 36)

(provide 'init-elpa)
;;; init-elpa.el ends here
