(require-package 'clojure-mode)

(require-package 'cider)

;(define-key cider-repl-mode-map (kbd "<home>") nil)
;(define-key cider-repl-mode-map (kbd "C-,") 'complete-symbol)
;(define-key cider-mode-map (kbd "C-,") 'complete-symbol)

;; Indent and highlight more commands
;(put-clojure-indent 'match 'defun)

;; Enable error buffer popping also in the REPL:
(setq cider-repl-popup-stacktraces t)

;; Specify history file
(setq cider-history-file "~/.emacs.d/nrepl-history")

;; auto-select the error buffer when it's displayed
(setq cider-auto-select-error-buffer t)

;; Prevent the auto-display of the REPL buffer in a separate window after connection is established
(setq cider-repl-pop-to-buffer-on-connect nil)

;; Enable eldoc in Clojure buffers
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

(provide 'init-clojure)
