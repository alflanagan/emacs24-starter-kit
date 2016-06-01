
(mapcar (lambda (fname)
          (check-declare-file fname))
        (cl-remove-if-not (lambda (fname)
                            (file-exists-p fname))
                          (mapcar (lambda(name)
                                    (replace-regexp-in-string "\\.org" ".el" name))
                                  (directory-files "~/.emacs.d/" nil ".*\\.org"))))

