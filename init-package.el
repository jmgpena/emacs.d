;;; init-package --- setup package settings and helper functions
;;; Commentary:

(require 'package)
(require 'dash)

;;; Code:

;; Add melpa to package repos
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)

(package-initialize)

(unless
    (and
     (file-exists-p "~/.emacs.d/elpa/archives/melpa")
     (file-exists-p "~/.emacs.d/elpa/archives/org")
     (file-exists-p "~/.emacs.d/elpa/archives/marmalade"))
  (package-refresh-contents))

(defun packages-install (packages)
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

(provide 'init-package)
;;; init-package.el ends here