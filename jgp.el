
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))

(defun jgp-load (file &optional header-or-tag)
  "Load configuration from other jgp-*.org files.
If the optional argument is the id of a subtree then only
configuration from within that subtree will be loaded.  If it is
not an id then it will be interpreted as a tag, and only subtrees
marked with the given tag will be loaded.

For example, to load all of ome-lisp.org simply
add (jgp-load \"lisp\") to your configuration.

To load only the 'window-system' config from
jgp-misc-recommended.org add
 (jgp-load \"misc-recommended\" \"window-system\")
to your configuration."
  (let ((file (expand-file-name (if (string-match "jgp-.+\.org" file)
                                    file
                                  (format "jgp-%s.org" file))
                                init-dir)))
    ;; ensure el-get-sources is empty before loading "jgp-.+\.org" files
    (setq el-get-sources nil)
    (org-babel-load-file
     (if header-or-tag
         (let* ((base (file-name-nondirectory file))
                (dir  (file-name-directory file))
                (partial-file (expand-file-name
                               (concat "." (file-name-sans-extension base)
                                       ".part." header-or-tag ".org")
                               dir)))
           (unless (file-exists-p partial-file)
             (with-temp-file partial-file
               (insert
                (with-temp-buffer
                  (insert-file-contents file)
                  (save-excursion
                    (condition-case nil ;; collect as a header
                        (progn
                          (org-link-search (concat"#"header-or-tag))
                          (org-narrow-to-subtree)
                          (buffer-string))
                      (error ;; collect all entries with as tags
                       (let (body)
                         (org-map-entries
                          (lambda ()
                            (save-restriction
                              (org-narrow-to-subtree)
                              (setq body (concat body "\n" (buffer-string)))))
                          header-or-tag)
                         body))))))))
           partial-file)
       file))
    (el-get 'sync (mapcar 'el-get-source-name el-get-sources))))

(jgp-load "gui")
(jgp-load "basic")
