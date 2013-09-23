;;; init.el --- emacs initialization

;; Setup proxy if it exists
(require 'url)
(let ((proxy (url-generic-parse-url (getenv "HTTP_PROXY"))))
  (unless (string= "" (url-host proxy))
    (setq url-proxy-services
	  `(("http" . ,(concat (url-host proxy) ":" (number-to-string (url-port proxy))))
	    ("https" . ,(concat (url-host proxy) ":"  (number-to-string (url-port proxy))))
	    ("ftp" . ,(concat (url-host proxy) ":" (number-to-string (url-port proxy))))))))

;; Debug
(setq debug-on-error t)

;; load the ome from the `after-init-hook' so all packages are loaded
(add-hook 'after-init-hook
 `(lambda ()
    ;; remember this directory
    (setq init-dir
          ,(file-name-directory (or load-file-name (buffer-file-name))))
    ;; load org mode
    (require 'org)
    ;; load up the ome
    (org-babel-load-file (expand-file-name "jgp.org" init-dir))))

;;; init.el ends here
