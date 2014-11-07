;;; init-fonts.el --- Font configuration
;;; Commentary:
;;;
;;; By jmgpena
;;;
;;; Code:

;; frame font
(if (and (boundp 'config:font-family)
         (display-graphic-p))
    (set-face-attribute
     'default nil :font (concat config:font-family "-" config:font-size)))

;; setup fallback font
(set-fontset-font "fontset-default" nil
                  (font-spec :size 20 :name "Symbola"))

(provide 'init-fonts)
;;; init-fonts.el ends here
