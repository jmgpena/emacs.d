;;; init-projectile --- projectile setup
;;; Commentary:
;;; Code:

(require-package 'projectile)
(require-package 'ag)
(setq ag-highlight-search t)

;; use projectile in project dirs
(projectile-global-mode)
;; enable caching
(setq projectile-enable-caching t)
;; integrate with helm
(global-set-key (kbd "C-c h") 'helm-projectile)

(provide 'init-projectile)
;;; init-projectile ends here