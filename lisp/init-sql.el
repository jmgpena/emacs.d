;;; init-sql.el --- sql mode configs
;;; Commentary:
;;;
;;; By jmgpena
;;;
;;; Code:

(require-package 'sql-indent)
(after-load 'sql
  (require 'sql-indent))

(defun site/pop-to-sqli-buffer ()
  "Switch to the corresponding sqli buffer."
  (interactive)
  (if sql-buffer
      (progn
        (pop-to-buffer sql-buffer)
        (goto-char (point-max)))
    (sql-set-sqli-buffer)
    (when sql-buffer
      (site/pop-to-sqli-buffer))))

(after-load 'sql
  (define-key sql-mode-map (kbd "C-c C-z") 'site/pop-to-sqli-buffer)
  (add-hook 'sql-interactive-mode-hook 'site/never-indent)
  (when (package-installed-p 'dash-at-point)
    (defun site/maybe-set-dash-db-docset ()
      (when (eq sql-product 'postgres)
        (setq dash-at-point-docset "psql")))

    (add-hook 'sql-mode-hook 'site/maybe-set-dash-db-docset)
    (add-hook 'sql-interactive-mode-hook 'site/maybe-set-dash-db-docset)
    (defadvice sql-set-product (after set-dash-docset activate)
      (site/maybe-set-dash-db-docset))))

(setq-default sql-input-ring-file-name
              (expand-file-name ".sqli_history" user-emacs-directory))

;; See my answer to https://emacs.stackexchange.com/questions/657/why-do-sql-mode-and-sql-interactive-mode-not-highlight-strings-the-same-way/673
(defun site/font-lock-everything-in-sql-interactive-mode ()
  (unless (eq 'oracle sql-product)
    (sql-product-font-lock nil nil)))
(add-hook 'sql-interactive-mode-hook 'site/font-lock-everything-in-sql-interactive-mode)


(after-load 'page-break-lines
  (push 'sql-mode page-break-lines-modes))

(provide 'init-sql)
;;; init-sql.el ends here
