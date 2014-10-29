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
;; (add-to-list 'package-archives
;;              '("marmalade" . "http://marmalade-repo.org/packages/") t)

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

;;  use fullframe for package list
(require-package 'fullframe)
;;(fullframe list-packages quit-window)

;; adjust the display of the package list
(require-package 'cl-lib)
(require 'cl-lib)

;; (defun site/set-tabulated-list-column-width (col-name width)
;;   "Set any column with name COL-NAME to the given WIDTH."
;;   (cl-loop for column across tabulated-list-format
;;            when (string= col-name (car column))
;;            do (setf (elt column 1) width)))

;; (defun site/maybe-widen-package-menu-columns ()
;;   "Widen some columns of the package menu table to avoid truncation."
;;   (when (boundp 'tabulated-list-format)
;;     (site/set-tabulated-list-column-width "Version" 13)
;;     (let ((longest-archive-name (apply 'max (mapcar 'length (mapcar 'car package-archives)))))
;;       (site/set-tabulated-list-column-width "Archive" longest-archive-name))))

;;(add-hook 'package-menu-mode-hook 'site/maybe-widen-package-menu-columns)

;; paradox package list replacement
(require-package 'paradox)
;; (setq paradox-github-token "08ceb37da5d8e34cfa6d2f74619d7a54fa0e1763"
;;       paradox-column-width-package 36)

(provide 'init-elpa)
;;; init-elpa.el ends here
