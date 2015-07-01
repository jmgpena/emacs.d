;;; init-clojure.el --- Clojure configs
;;; Commentary:
;;;
;;; By jmgpena
;;;
;;; Code:

(require-package 'clojure-mode)
(require-package 'cljsbuild-mode)
(require-package 'cider)

(after-load 'clojure-mode
  (add-hook 'clojure-mode-hook 'site/lisp-setup)
  (add-hook 'clojure-mode-hook 'subword-mode)
  (font-lock-add-keywords
   'clojure-mode `(("(\\(fn\\)[\[[:space:]]"
                    (0 (progn (compose-region (match-beginning 1)
                                              (match-end 1) "λ")
                              nil)))))
  (font-lock-add-keywords
   'clojure-mode `(("\\(#\\)("
                    (0 (progn (compose-region (match-beginning 1)
                                              (match-end 1) "λ")
                              nil)))))
  (font-lock-add-keywords
   'clojure-mode `(("\\(#\\){"
                    (0 (progn (compose-region (match-beginning 1)
                                              (match-end 1) "∈")
                              nil))))))

;; Use clojure-mode for clojurescript, since clojurescript-mode
;; pulls in Slime
(add-auto-mode 'clojure-mode "\\.cljs\\'")

;;; clojure refactor mode
;; (require-package 'clj-refactor)
;; (add-hook 'clojure-mode-hook (lambda ()
;;                                (clj-refactor-mode 1)
;;                                (cljr-add-keybindings-with-prefix "C-c C-m")))

;; (define-key clojure-mode-map (kbd "C-:") 'cljr-cycle-stringlike)
;; (define-key clojure-mode-map (kbd "C->") 'cljr-cycle-coll)

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
;;(setq cider-repl-pop-to-buffer-on-connect nil)

;; Enable eldoc in Clojure buffers
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

 ;; nrepl isn't based on comint
(add-hook 'cider-repl-mode-hook
          (lambda () (setq show-trailing-whitespace nil)))

(provide 'init-clojure)
;;; init-clojure.el ends here
