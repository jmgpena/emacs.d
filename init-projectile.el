;; use projectile in project dirs
(projectile-global-mode)
;; enable caching
(setq projectile-enable-caching t)
;; integrate with helm
(global-set-key (kbd "C-c h") 'helm-projectile)
