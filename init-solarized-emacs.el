(setq solarized-broken-srgb nil)

(setq solarized-distinct-fringe-background t)
(setq solarized-high-contrast-mode-line t)
(setq solarized-use-variable-pitch nil)

(load-theme 'solarized-dark t)
;; whitespace mode customization
;; (custom-set-faces
;;  '(whitespace-space ((t (:background "#002b36"))))
;;  '(whitespace-tab ((t (:foreground "#586e75" :background "#002b36"))))
;;  '(whitespace-newline ((t (:foreground "#073642" :background "#002b36"))))
;; )
;; (add-hook 'after-make-frame-functions
;;           '(lambda (f)
;;              (with-selected-frame f
;;                (when (window-system) (load-theme 'solarized-dark t)))))
