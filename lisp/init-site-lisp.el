;;; init-site-lisp.el --- init site-lisp dir
;;; Commentary:
;;;
;;; By jmgpena
;;;
;;; Code:

(eval-when-compile (require 'cl))

;; Set path to elisp dependencies
(defvar site-lisp-dir
    (expand-file-name "site-lisp" user-emacs-directory))
(add-to-list 'load-path site-lisp-dir)

;; Add external projects to load path
(dolist (subdir (directory-files site-lisp-dir t "\\w+"))
  (when (file-directory-p subdir)
    (add-to-list 'load-path subdir)))

(provide 'init-site-lisp)
;;; init-site-lisp.el ends here
