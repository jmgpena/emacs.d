
;; believe me, you don't need menubar, toolbar nor scrollbar
(dolist (mode '( tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

;; Don't show splash screen
(setq inhibit-startup-message t)

;; show column number and line number
; column-number-mode: show column number in modeline
; line-number-mode: show line number in modeline
; global-linum-mode: shown line numbers in left column
(dolist (mode '(column-number-mode line-number-mode))
  (when (fboundp mode) (funcall mode t)))

;; make the fringe thinner (default is 8 in pixels)
(fringe-mode 4)

;; show parenthesis match
(show-paren-mode 1)
(setq show-paren-style 'expression)

;; Toggle line highlighting in all buffers
(global-hl-line-mode t)

; whitespace settings
(require 'whitespace)
(setq whitespace-style '(face spaces tabs newline space-mark tab-mark
                              newline-mark lines-tail trailing))
(setq whitespace-display-mappings
      '((space-mark 32 [183] [46])
        (newline-mark 10 [182 10])
        (tab-mark 9 [9655 9] [92 9])))
(setq whitespace-line-column 80)
(global-whitespace-mode t)
(setq whitespace-global-modes '(prog-mode emacs-lisp-mode))

;; navigate windows with M-<arrows>
(windmove-default-keybindings 'meta)
(setq windmove-wrap-around t)

;; frame font
(if (member config:font-family (font-family-list))
    (set-face-attribute
     'default nil :font (concat config:font-family " " config:font-size)))