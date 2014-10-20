;;; init-projectile --- projectile setup
;;; Commentary:
;;; Code:

(require-package 'projectile)
(require-package 'ag)
(require-package 'helm-projectile)

(setq ag-highlight-search t)

;; use projectile in project dirs
(projectile-global-mode)
;; enable caching
(setq projectile-enable-caching t)

;; helm-projectile
(require 'helm-projectile)
(helm-projectile-on)

(defun projectile-helm-ag ()
  (interactive)
  (helm-ag (projectile-project-root)))

(provide 'init-projectile)
;;; init-projectile ends here
