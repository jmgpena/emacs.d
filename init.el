;;; Emacs init file

;; Setup proxy if it exists
(require 'url)
(let ((proxy (url-generic-parse-url (getenv "HTTP_PROXY"))))
  (unless (string= "" (url-host proxy))
    (setq url-proxy-services
	  `(("http" . ,(concat (url-host proxy) ":" (number-to-string (url-port proxy))))
	    ("https" . ,(concat (url-host proxy) ":"  (number-to-string (url-port proxy))))
	    ("ftp" . ,(concat (url-host proxy) ":" (number-to-string (url-port proxy))))))))

