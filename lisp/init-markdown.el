;;; init-markdown.el --- Markdown config
;;; Commentary:
;;;
;;; By jmgpena
;;;
;;; Code:

(require-package 'markdown-mode)

(setq auto-mode-alist
      (cons '("\\.\\(md\\|markdown\\)\\'" . markdown-mode) auto-mode-alist))

(setq markdown-command "pandoc -f markdown -t pdf")
(provide 'init-markdown)
;;; init-markdown.el ends here
