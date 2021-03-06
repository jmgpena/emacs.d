;;; init-vc.el --- Version control configs
;;; Commentary:
;;;
;;; By jmgpena
;;;
;;; Code:

;;;----------------------------------------------------------------------------
;;; Highlight diffs in realtime
;;;----------------------------------------------------------------------------
(require-package 'diff-hl)
(global-diff-hl-mode) ; use globally
;;(add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)
;;(add-hook 'vc-dir-mode-hook 'turn-on-diff-hl-mode)
;; Alternative modes:
;; https://github.com/syohex/emacs-git-gutter (indicators on margin, commit hunk)
;; https://github.com/syohex/emacs-git-gutter-fringe

;;;----------------------------------------------------------------------------
;;; Git configurations
;;;----------------------------------------------------------------------------
(require-package 'magit)
(require-package 'git-blame)
(require-package 'gitignore-mode)
(require-package 'gitconfig-mode)
(require-package 'git-messenger) ;; Though see also vc-annotate's "n" & "p" bindings
(require-package 'git-timemachine)

(setq-default
 magit-save-some-buffers nil
 magit-process-popup-time 10
 magit-diff-refine-hunk t
 ;magit-completing-read-function 'magit-ido-completing-read
 )
(setq magit-last-seen-setup-instructions "1.4.0")

;; Hint: customize `magit-repo-dirs' so that you can use C-u M-F12 to
;; quickly open magit on any one of your projects.
(global-set-key [(meta f12)] 'magit-status)
(global-set-key (kbd "C-x m") 'magit-status)

(after-load 'magit
  (define-key magit-status-mode-map (kbd "C-M-<up>") 'magit-goto-parent-section))

(require-package 'fullframe)
(after-load 'magit
  (fullframe magit-status magit-mode-quit-window))

(add-hook 'git-commit-mode-hook 'goto-address-mode)
;;(after-load 'session
;;  (add-to-list 'session-mode-disable-list 'git-commit-mode))

(when *is-a-mac*
  (after-load 'magit
    (add-hook 'magit-mode-hook (lambda () (local-unset-key [(meta h)])))))

;; Convenient binding for vc-git-grep
(global-set-key (kbd "C-x v f") 'vc-git-grep)

;;; git-svn support
;(require-package 'magit-svn)
;(autoload 'magit-svn-enabled "magit-svn")
;(defun site/maybe-enable-magit-svn-mode ()
;  (when (magit-svn-enabled)
;    (magit-svn-mode)))
;(add-hook 'magit-status-mode-hook #'site/maybe-enable-magit-svn-mode)

(after-load 'compile
  (dolist (defn (list '(git-svn-updated "^\t[A-Z]\t\\(.*\\)$" 1 nil nil 0 1)
                      '(git-svn-needs-update "^\\(.*\\): needs update$" 1 nil nil 2 1)))
    (add-to-list 'compilation-error-regexp-alist-alist defn)
    (add-to-list 'compilation-error-regexp-alist (car defn))))

(defvar git-svn--available-commands nil "Cached list of git svn subcommands")

(defun git-svn (dir)
  "Run a git svn subcommand in DIR."
  (interactive "DSelect directory: ")
  (unless git-svn--available-commands
    (setq git-svn--available-commands
          (site/string-all-matches
           "^  \\([a-z\\-]+\\) +"
           (shell-command-to-string "git svn help") 1)))
  (let* ((default-directory (vc-git-root dir))
         (compilation-buffer-name-function (lambda (major-mode-name) "*git-svn*")))
    (compile (concat "git svn "
                     (ido-completing-read "git-svn command: " git-svn--available-commands nil t)))))

(require-package 'git-messenger)
(global-set-key (kbd "C-x v p") #'git-messenger:popup-message)

;;;----------------------------------------------------------------------------
;;; Github
;;;----------------------------------------------------------------------------
(require-package 'yagist)
(require-package 'github-browse-file)
(require-package 'bug-reference-github)
(add-hook 'prog-mode-hook 'bug-reference-prog-mode)

(when (eval-when-compile (> emacs-major-version 23))
  (require-package 'github-clone)
  (require-package 'magit-gh-pulls))

(provide 'init-vc)
;;; init-vc.el ends here
