;;; init.el --- emacs initialization
;;; Commentary:
;;;
;;; This file loads the main configuration file in org format
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

;; locale settings and coding
(setq system-time-locale "C")
(setq locale-coding-system 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8-unix)
(set-selection-coding-system 'utf-8-unix)
;; set environment coding system
(set-language-environment "UTF-8")

;; bootstrap the package system and the latest version of org-mode
;; use package.el from emacs
(require 'package)
(setq package-check-signature nil)

;; Define package archives
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives
             '("SC" . "http://joseito.republika.pl/sunrise-commander/"))

;; If gpg cannot be found, signature checking will fail, so we
;; conditionally enable it according to whether gpg is available. We
;; re-run this check once $PATH has been configured
;; (defun site/package-maybe-enable-signatures ()
;;   "Enable package signatures if gpg is present."
;;   (setq package-check-signature (when (executable-find "gpg") 'allow-unsigned)))

;; (site/package-maybe-enable-signatures)
;; (after-load 'init-exec-path
;;   (site/package-maybe-enable-signatures))

;; initialize package system
(package-initialize)
(setq package-enable-at-startup nil)

(unless
    (and
     (file-exists-p "~/.emacs.d/elpa/archives/melpa")
     (file-exists-p "~/.emacs.d/elpa/archives/org")
     (file-exists-p "~/.emacs.d/elpa/archives/SC"))
  (package-refresh-contents))

;; bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(setq use-package-always-ensure t)
(require 'use-package)

;; load latest version of org-mode
(use-package org-plus-contrib
  :defer t)

(org-babel-load-file "~/.emacs.d/jmgpena.org")

(provide 'init)
;;; init.el ends here
