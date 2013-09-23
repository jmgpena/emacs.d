
;; believe me, you don't need menubar, toolbar nor scrollbar
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

;; show column number and line number
(dolist (mode '(column-number-mode line-number-mode global-linum-mode))
  (when (fboundp mode) (funcall mode t)))

;; make the fringe thinner (default is 8 in pixels)
(fringe-mode 4)

;; show parenthesis match
(show-paren-mode 1)
(setq show-paren-style 'expression)

;; Toggle line highlighting in all buffers
(global-hl-line-mode t)

(add-to-list 'el-get-sources
             '(:name solarized-emacs
                     :type github
                     :pkgname "bbatsov/solarized-emacs"
                     :description "Solarized themes for Emacs"
                     :prepare (add-to-list 'custom-theme-load-path default-directory)
                     :after (progn
                              (setq solarized-broken-srgb nil)
                              (load-theme 'solarized-dark t))))
