;;; init-recentf.el --- recentf configuration
;;; Commentary:
;;;
;;; By jmgpena
;;;
;;; Code:

;;; recent files
(require 'recentf)
;; save the .recentf file to .emacs.d/
(setq recentf-save-file (concat user-emacs-directory ".recentf"))
;; enable recent files mode.
(recentf-mode t)
;; 1000 files ought to be enough.
(setq recentf-max-saved-items 1000
      recentf-exclude '("/tmp/" "/ssh:"))

(provide 'init-recentf)
;;; init-recentf.el ends here
