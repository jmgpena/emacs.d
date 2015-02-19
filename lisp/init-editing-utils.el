;;; init-editing-utils.el --- Editing configs
;;; Commentary:
;;;
;;; By jmgpena
;;;
;;; Code:

;;;----------------------------------------------------------------------------
;;; Default settings
;;;----------------------------------------------------------------------------

(setq-default
 cursor-type 'bar
 bookmark-default-file (expand-file-name ".bookmarks.el" user-emacs-directory)
 buffers-menu-max-size 30
 case-fold-search t
 line-number-mode t
 column-number-mode t
 delete-selection-mode t
 delete-selection-mode t
 ediff-diff-options "-w"
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
 ring-bell-function 'ignore
 apropos-do-all t
 mouse-yank-at-point t)

;; Transparently open compressed files
(auto-compression-mode t)

;; set TAB and indention
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
;; y or n is suffice for a yes or no question
(fset 'yes-or-no-p 'y-or-n-p)
;; set text-mode as the default major mode, instead of fundamental-mode
;; The first of the two lines in parentheses tells Emacs to turn on Text mode
;; when you find a file, unless that file should go into some other mode, such
;; as C mode.
(setq-default major-mode 'text-mode)
;; make scripts executable by default
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)
;;;; save last point in files
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (concat user-emacs-directory "saved-places"))
;; Toggle line highlighting in all buffers
(global-hl-line-mode t)

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

;;;----------------------------------------------------------------------------
;;; Whitespace settings
;;;----------------------------------------------------------------------------
; whitespace settings
(require 'whitespace)
(setq whitespace-style '(face spaces tabs newline space-mark tab-mark
                              newline-mark lines-tail trailing))
(setq whitespace-display-mappings
      '((space-mark 32 [183] [46])
        (newline-mark 10 [182 10])
        (tab-mark 9 [9655 9] [92 9])))
(setq whitespace-line-column 80)

;;----------------------------------------------------------------------------
;; Don't disable narrowing commands
;;----------------------------------------------------------------------------
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

;;----------------------------------------------------------------------------
;; Show matching parens
;;----------------------------------------------------------------------------
(show-paren-mode 1)

;;----------------------------------------------------------------------------
;; Expand region
;;----------------------------------------------------------------------------
(require-package 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;;----------------------------------------------------------------------------
;; Don't disable case-change functions
;;----------------------------------------------------------------------------
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)


;;----------------------------------------------------------------------------
;; Rectangle selections, and overwrite text when the selection is active
;;----------------------------------------------------------------------------
(cua-selection-mode t)                  ; for rectangles, CUA is nice

;;----------------------------------------------------------------------------
;; Handy key bindings
;;----------------------------------------------------------------------------
;; To be able to M-x without meta
(global-set-key (kbd "C-x C-m") 'execute-extended-command)

;; Bind revert-buffer to C-x C-r
(global-set-key (kbd "C-x C-r") (lambda () (interactive) (revert-buffer nil t)))

;; Vimmy alternatives to M-^ and C-u M-^
(global-set-key (kbd "C-c j") 'join-line)
(global-set-key (kbd "C-c J") (lambda () (interactive) (join-line 1)))

(global-set-key (kbd "C-.") 'set-mark-command)
(global-set-key (kbd "C-x C-.") 'pop-global-mark)

;; searching and jumping around
(use-package ace-jump-mode
  :ensure
  :bind (("C-;" . ace-jump-mode)
         ("C-:" . ace-jump-word-mode)))
(use-package helm-swoop :ensure)
(use-package ace-isearch
  :ensure
  :init
  (global-ace-isearch-mode +1))

(require-package 'multiple-cursors)
;; multiple-cursors
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-+") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
;; From active region to multiple cursors:
(global-set-key (kbd "C-c c r") 'set-rectangular-region-anchor)
(global-set-key (kbd "C-c c c") 'mc/edit-lines)
(global-set-key (kbd "C-c c e") 'mc/edit-ends-of-lines)
(global-set-key (kbd "C-c c a") 'mc/edit-beginnings-of-lines)

;; automatically indent after return
(define-key global-map (kbd "RET") 'newline-and-indent)

;;;----------------------------------------------------------------------------
;;; Kill back to indentation
;;;----------------------------------------------------------------------------
(defun kill-back-to-indentation ()
  "Kill from point back to the first non-whitespace character on the line."
  (interactive)
  (let ((prev-pos (point)))
    (back-to-indentation)
    (kill-region (point) prev-pos)))

(global-set-key (kbd "C-M-<backspace>") 'kill-back-to-indentation)

;;----------------------------------------------------------------------------
;; Page break lines
;;----------------------------------------------------------------------------
(require-package 'page-break-lines)
(global-page-break-lines-mode)

;;----------------------------------------------------------------------------
;; Fill column indicator
;;----------------------------------------------------------------------------
(when (eval-when-compile (> emacs-major-version 23))
  (require-package 'fill-column-indicator)
  (defun site/prog-mode-fci-settings ()
    (turn-on-fci-mode)
    (when show-trailing-whitespace
      (set (make-local-variable 'whitespace-style) '(face trailing))
      (whitespace-mode 1)))

  ;;(add-hook 'prog-mode-hook 'site/prog-mode-fci-settings)

  (defun site/fci-enabled-p ()
    (and (boundp 'fci-mode) fci-mode))

  (defvar site/fci-mode-suppressed nil)
  (defadvice popup-create (before suppress-fci-mode activate)
    "Suspend fci-mode while popups are visible"
    (let ((fci-enabled (site/fci-enabled-p)))
      (when fci-enabled
        (set (make-local-variable 'site/fci-mode-suppressed) fci-enabled)
        (turn-off-fci-mode))))
  (defadvice popup-delete (after restore-fci-mode activate)
    "Restore fci-mode when all popups have closed"
    (when (and site/fci-mode-suppressed
               (null popup-instances))
      (setq site/fci-mode-suppressed nil)
      (turn-on-fci-mode)))

  ;; Regenerate fci-mode line images after switching themes
  (defadvice enable-theme (after recompute-fci-face activate)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (site/fci-enabled-p)
          (turn-on-fci-mode))))))

;; set fill-column for text mode
(setq-default fill-column 79)
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;;----------------------------------------------------------------------------
;; Shift lines up and down with M-up and M-down. When paredit is enabled,
;; it will use those keybindings. For this reason, you might prefer to
;; use M-S-up and M-S-down, which will work even in lisp modes.
;;----------------------------------------------------------------------------
(require-package 'move-dup)
(global-set-key [M-up] 'md/move-lines-up)
(global-set-key [M-down] 'md/move-lines-down)
(global-set-key [M-S-up] 'md/move-lines-up)
(global-set-key [M-S-down] 'md/move-lines-down)

(global-set-key (kbd "C-c p") 'md/duplicate-down)
(global-set-key (kbd "C-c P") 'md/duplicate-up)

;;----------------------------------------------------------------------------
;; Fix backward-up-list to understand quotes, see http://bit.ly/h7mdIL
;;----------------------------------------------------------------------------
(defun backward-up-sexp (arg)
  "Jump up to the start of the ARG'th enclosing sexp."
  (interactive "p")
  (let ((ppss (syntax-ppss)))
    (cond ((elt ppss 3)
           (goto-char (elt ppss 8))
           (backward-up-sexp (1- arg)))
          ((backward-up-list arg)))))

(global-set-key [remap backward-up-list] 'backward-up-sexp) ; C-M-u, C-M-up

;;----------------------------------------------------------------------------
;; Cut/copy the current line if no region is active
;;----------------------------------------------------------------------------
(require-package 'whole-line-or-region)
(whole-line-or-region-mode t)
(make-variable-buffer-local 'whole-line-or-region-mode)

(defun suspend-mode-during-cua-rect-selection (mode-name)
  "Add an advice to suspend `MODE-NAME' while selecting a CUA rectangle."
  (let ((flagvar (intern (format "%s-was-active-before-cua-rectangle" mode-name)))
        (advice-name (intern (format "suspend-%s" mode-name))))
    (eval-after-load 'cua-rect
      `(progn
         (defvar ,flagvar nil)
         (make-variable-buffer-local ',flagvar)
         (defadvice cua--activate-rectangle (after ,advice-name activate)
           (setq ,flagvar (and (boundp ',mode-name) ,mode-name))
           (when ,flagvar
             (,mode-name 0)))
         (defadvice cua--deactivate-rectangle (after ,advice-name activate)
           (when ,flagvar
             (,mode-name 1)))))))

(suspend-mode-during-cua-rect-selection 'whole-line-or-region-mode)

;;;----------------------------------------------------------------------------
;;; Highlight escape sequence
;;;----------------------------------------------------------------------------

(require-package 'highlight-escape-sequences)
(hes-mode)

;;;----------------------------------------------------------------------------
;;; Guide key mode
;;;----------------------------------------------------------------------------
(require-package 'guide-key)
(setq guide-key/guide-key-sequence '("C-x r" "C-x 4" "C-x 5" "C-c ;" "C-c ; f" "C-c ' f" "C-x n" "C-c p" "C-x 8" "C-x v" "C-c h"))
(setq guide-key/recursive-key-sequence-flag t)
(setq guide-key/popup-window-position 'bottom)
(guide-key-mode 1)

;; reload changed files
(global-auto-revert-mode)
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)

(transient-mark-mode t)

;;;----------------------------------------------------------------------------
;;; Smart move to beginning of line
;;;----------------------------------------------------------------------------
(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'smarter-move-beginning-of-line)

;;; Agressive indent mode
(require-package 'aggressive-indent)
(global-aggressive-indent-mode 1)
;; (add-to-list 'aggressive-indent-excluded-modes 'html-mode)

(provide 'init-editing-utils)
;;; init-editing-utils.el ends here
