;;; init-editing-utils.el --- Editing configs
;;; Commentary:
;;;
;;; By jmgpena
;;;
;;; Code:

(setq-default
 blink-cursor-delay 0
 blink-cursor-interval 0.4
 bookmark-default-file (expand-file-name ".bookmarks.el" user-emacs-directory)
 buffers-menu-max-size 30
 case-fold-search t
 line-number-mode t
 column-number-mode t
 delete-selection-mode t
 delete-selection-mode t
 ediff-split-window-function 'split-window-horizontally
 ediff-window-setup-function 'ediff-setup-windows-plain
 indent-tabs-mode nil
 make-backup-files nil
 mouse-yank-at-point t
 save-interprogram-paste-before-kill t
 scroll-preserve-screen-position 'always
 set-mark-command-repeat-pop t
 show-trailing-whitespace t
 next-line-add-newlines nil
 require-final-newline t
 highlight-tabs t
 tooltip-delay 1.5
 truncate-lines nil
 truncate-partial-width-windows nil
 visible-bell t
 ring-bell-function 'ignore)

;; remove trailing whitespaces before save
;; this package does this in a better way
(require-package 'whitespace-cleanup-mode)
(global-whitespace-cleanup-mode t)

;; prettify symbols
(when (fboundp 'global-prettify-symbols-mode)
  (global-prettify-symbols-mode))

;; undo-tree
(require-package 'undo-tree)
(global-undo-tree-mode)

;; highlight current symbol
(require-package 'highlight-symbol)
(dolist (hook '(prog-mode-hook html-mode-hook css-mode-hook))
  (add-hook hook 'highlight-symbol-mode)
  (add-hook hook 'highlight-symbol-nav-mode))

(require-package 'browse-kill-ring)

;; enable to support navigate in camelCase words
(global-subword-mode t)
;; make the fringe thinner (default is 8 in pixels)
(fringe-mode 4)
;; Toggle line highlighting in all buffers
(global-hl-line-mode t)

;; reload changed files
(global-auto-revert-mode)
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)

(transient-mark-mode t)

(provide 'init-editing-utils)
;;; init-editing-utils.el ends here
