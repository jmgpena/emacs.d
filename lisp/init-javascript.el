;;; init-javascript.el --- Javascript
;;; Commentary:
;;;
;;; By jmgpena
;;;
;;; Code:

;; packages
(require-package 'js2-mode)
(require-package 'js2-refactor)
(require-package 'web-beautify)
(require-package 'json-mode)
(require-package 'json-reformat)
(require-package 'coffee-mode)

;; Autoloads
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jshintrc$" . json-mode))
(add-to-list 'magic-mode-alist '("#!/usr/bin/env node" . js2-mode))

;; setup imenu
(after-load 'js2-mode
  (js2-imenu-extras-setup)
  (define-key js2-mode-map (kbd "C-c b") 'web-beautify-js))

;; settings
(setq-default
 js2-allow-rhino-new-expr-initializer nil
 js2-auto-indent-p nil
 js2-enter-indents-newline nil
 js2-global-externs
    '("module" "require" "buster" "sinon" "assert" "refute" "setTimeout"
         "clearTimeout" "setInterval" "clearInterval" "location" "__dirname"
         "console" "JSON" "Ext")
 js2-include-jslint-globals t
 js2-idle-timer-delay 0.1
 js2-indent-on-enter-key nil
 js2-mirror-mode nil
 js2-strict-inconsistent-return-warning nil
 js2-auto-indent-p t
 js2-include-rhino-externs nil
 js2-include-gears-externs nil
 js2-concat-multiline-strings 'eol
 js2-rebind-eol-bol-keys nil
 js2-highlight-level 3
 ;; let flycheck parse errors
 js2-show-parse-errors nil
 js2-strict-missing-semi-warning nil
 js2-show-strict-warnings nil
 ;; jshint does not report this?????
 js2-strict-trailing-comma-warning t)

(add-hook 'js2-mode-hook (lambda ()
                             (flycheck-select-checker 'javascript-jshint)
                             (flycheck-mode)))
(add-hook 'js2-mode-hook 'color-identifiers-mode)

;; Use lambda for anonymous font
(font-lock-add-keywords
 'js2-mode `(("\\(function\\) *("
              (0 (progn (compose-region (match-beginning 1)
                                        (match-end 1) "\u0192")
                        nil)))))

;; Use right arrow for return in one-line functions
(font-lock-add-keywords
 'js2-mode `(("function *([^)]*) *{ *\\(return\\) "
              (0 (progn (compose-region (match-beginning 1)
                                        (match-end 1) "\u2190")
                        nil)))))

(font-lock-add-keywords 'js2-mode
                        `(("\\<\\(FIX\\|TODO\\|FIXME\\|HACK\\|REFACTOR\\):"
                           1 font-lock-warning-face t)))

(require 'json)

;; parse jshintrc for more globals
(defun jp-js2-parse-jshintrc ()
    "This looks recursively up for a .jshintrc and extracts the
globals from it to add them to js2-additional-externs."
    (let* ((jshintrc (expand-file-name
                         ".jshintrc"
                         (locate-dominating-file default-directory ".jshintrc")))
              (json (and (file-readable-p jshintrc)
                        (json-read-file jshintrc)))
              (globals (and json
                           (cdr (assq 'globals json))))
              )
        (when globals
            (setq js2-additional-externs
                (append
                    (mapcar (lambda (pair)
                                (symbol-name (car pair)))
                        globals)
                    js2-additional-externs)))))

(defun jp-js2-flycheck-jshintrc ()
    "Documentation."
    (let* ((jshintrc (expand-file-name
                         ".jshintrc"
                         (locate-dominating-file default-directory ".jshintrc"))))
        (when (file-readable-p jshintrc)
            (setq flycheck-jshintrc jshintrc))))

(add-hook 'js2-init-hook 'jp-js2-flycheck-jshintrc)

;; auto-completion
;(require-package 'ac-js2)
;(add-hook 'js2-mode-hook 'ac-js2-mode)

;;;----------------------------------------------------------------------------
;;; Skewer repl
;;;----------------------------------------------------------------------------
(require-package 'skewer-mode)

(require 'skewer-repl)
(require 'skewer-html)
(require 'skewer-css)

(skewer-setup)

(require-package 'nodejs-repl)

(provide 'init-javascript)
;;; init-javascript.el ends here
