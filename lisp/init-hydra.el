(use-package hydra
  :ensure t
  :init
  (progn
    (defhydra hydra-buffer-menu (:color pink)
      "
  Mark               Unmark             Actions            Search
-------------------------------------------------------------------------
_m_: mark          _u_: unmark        _x_: execute       _R_: re-isearch
_s_: save          _U_: unmark up     _b_: bury          _I_: isearch
_d_: delete                           _g_: refresh       _O_: multi-occur
_D_: delete up                        _T_: files only: %`Buffer-menu-files-only
_~_: modified
"
      ("m" Buffer-menu-mark nil)
      ("u" Buffer-menu-unmark nil)
      ("U" Buffer-menu-backup-unmark nil)
      ("d" Buffer-menu-delete nil)
      ("D" Buffer-menu-delete-backwards nil)
      ("s" Buffer-menu-save nil)
      ("~" Buffer-menu-not-modified nil)
      ("x" Buffer-menu-execute nil)
      ("b" Buffer-menu-bury nil)
      ("g" revert-buffer nil)
      ("T" Buffer-menu-toggle-files-only nil)
      ("O" Buffer-menu-multi-occur nil :color blue)
      ("I" Buffer-menu-isearch-buffers nil :color blue)
      ("R" Buffer-menu-isearch-buffers-regexp nil :color blue)
      ("c" nil "cancel")
      ("v" Buffer-menu-select "select" :color blue)
      ("o" Buffer-menu-other-window "other-window" :color blue)
      ("q" quit-window "quit" :color blue))

    (define-key Buffer-menu-mode-map "." 'hydra-buffer-menu/body)))

(provide 'init-hydra)
