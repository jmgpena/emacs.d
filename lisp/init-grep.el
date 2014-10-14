;;; init-grep.el --- Grep and friends config
;;; Commentary:
;;;
;;; By jmgpena
;;;
;;; Code:

(require-package 'wgrep)

(setq-default grep-highlight-matches t
              grep-scroll-output t)

(when *is-a-mac*
  (setq-default locate-command "mdfind"))

(when (executable-find "ag")
  (require-package 'ag)
  (require-package 'wgrep-ag)
  (autoload 'wgrep-ag-setup "wgrep-ag")
  (add-hook 'ag-mode-hook 'wgrep-ag-setup)
  (setq-default ag-highlight-search t)
  (global-set-key (kbd "M-?") 'ag-project))

(provide 'init-grep)
;;; init-grep.el ends here
