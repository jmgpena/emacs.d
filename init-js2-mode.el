;;(rename-modeline "js2-mode" js2-mode "JS2")

(setq-default js2-allow-rhino-new-expr-initializer nil)
(setq-default js2-auto-indent-p nil)
(setq-default js2-enter-indents-newline nil)
(setq-default js2-global-externs
    '("module" "require" "buster" "sinon" "assert" "refute" "setTimeout"
         "clearTimeout" "setInterval" "clearInterval" "location" "__dirname"
         "console" "JSON" "Ext"))
(setq-default js2-idle-timer-delay 0.1)
(setq-default js2-indent-on-enter-key nil)
(setq-default js2-mirror-mode nil)
(setq-default js2-strict-inconsistent-return-warning nil)
(setq-default js2-auto-indent-p t)
(setq-default js2-include-rhino-externs nil)
(setq-default js2-include-gears-externs nil)
(setq-default js2-concat-multiline-strings 'eol)
(setq-default js2-rebind-eol-bol-keys nil)

;; syntax hl level
(setq js2-highlight-level 3)

;; Let flycheck handle parse errors
(setq-default js2-show-parse-errors nil)
(setq-default js2-strict-missing-semi-warning nil)
(setq-default js2-show-strict-warnings nil)
;; jshint does not warn about this now for some reason
(setq-default js2-strict-trailing-comma-warning t)

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

;;
;; (defun jp-js2-parse-jshintrc ()
;;   "This looks recursively up for a .jshintrc and extracts the
;; globals from it to add them to js2-additional-externs."
;;   (let* ((jshintrc (locate-dominating-file default-directory "^\\.jshintrc$"))
;;             (json (and jshintrc
;;                       (json-read-file (car jshintrc))))
;;             (globals (and json
;;                          (cdr (assq 'globals json))))
;;             )
;;       (when globals
;;           (setq js2-additional-externs
;;               (append
;;                   (mapcar (lambda (pair)
;;                               (symbol-name (car pair))
;;                               )
;;                       globals
;;                       )
;;                   js2-additional-externs
;;                   )
;;               )
;;           (js2-reparse t)
;;           )
;;       )
;;     )
;; (add-hook 'js2-init-hook 'jp-js2-parse-jshintrc)

;; auto-completion
;(require-package 'ac-js2)
;(add-hook 'js2-mode-hook 'ac-js2-mode)

(provide 'init-js2-mode)
