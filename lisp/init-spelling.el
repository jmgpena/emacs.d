;;; init-spelling.el --- Spelling config
;;; Commentary:
;;;
;;; By jmgpena
;;;
;;; Code:

;; use aspell instead of ispell
(setq-default ispell-program-name "aspell"
              ispell-extra-args '("--sug-mode=ultra"))

(provide 'init-spelling)
;;; init-spelling.el ends here
