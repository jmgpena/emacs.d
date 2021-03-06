;;; init-ruby.el --- Ruby lang configs
;;; Commentary:
;;;
;;; By jmgpena
;;;
;;; Code:

;;; Basic ruby setup
(require-package 'ruby-hash-syntax)

(add-auto-mode 'ruby-mode
               "Rakefile\\'" "\\.rake\\'" "\\.rxml\\'"
               "\\.rjs\\'" "\\.irbrc\\'" "\\.pryrc\\'" "\\.builder\\'" "\\.ru\\'"
               "\\.gemspec\\'" "Gemfile\\'" "Kirkfile\\'" "Vagrantfile\\'")

(setq ruby-use-encoding-map nil)

(after-load 'ruby-mode
  (define-key ruby-mode-map (kbd "RET") 'reindent-then-newline-and-indent)
  (define-key ruby-mode-map (kbd "TAB") 'indent-for-tab-command)

  ;; Stupidly the non-bundled ruby-mode isn't a derived mode of
  ;; prog-mode: we run the latter's hooks anyway in that case.
  (add-hook 'ruby-mode-hook
            (lambda ()
              (unless (derived-mode-p 'prog-mode)
                (run-hooks 'prog-mode-hook)))))

(add-hook 'ruby-mode-hook 'subword-mode)

;; TODO: hippie-expand ignoring : for names in ruby-mode
;; TODO: hippie-expand adaptor for auto-complete sources
;;; Inferior ruby
(require-package 'inf-ruby)
;; (require-package 'ac-inf-ruby)
;; (after-load 'auto-complete
;;   (add-to-list 'ac-modes 'inf-ruby-mode))
;; (add-hook 'inf-ruby-mode-hook 'ac-inf-ruby-enable)
;; (after-load 'inf-ruby
;;   (define-key inf-ruby-mode-map (kbd "TAB") 'auto-complete))

;;; Ruby compilation
(require-package 'ruby-compilation)

(after-load 'ruby-mode
  (let ((m ruby-mode-map))
    (define-key m [S-f7] 'ruby-compilation-this-buffer)
    (define-key m [f7] 'ruby-compilation-this-test)
    (define-key m [f6] 'recompile)))

;;; Robe
(require-package 'robe)
(after-load 'ruby-mode
  (add-hook 'ruby-mode-hook 'robe-mode))

;;; ri support
(require-package 'yari)
(defalias 'ri 'yari)

;;; YAML
(require-package 'yaml-mode)

;; rvm integration
(require-package 'rvm)
(rvm-use-default)

(provide 'init-ruby)
;;; init-ruby.el ends here
