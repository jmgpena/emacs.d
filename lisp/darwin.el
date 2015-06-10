;;; darwin.el --- osx specific config
;;; commentary:
;;; code:

;; todo vars to config
(setenv "TMPDIR" "/tmp")

;; config variables
(setq config:font-family "Inconsolata")
(setq config:font-size "15")

;; use gnu ls for dired
(setq insert-directory-program (executable-find "gls"))
;;; darwin.el ends here
