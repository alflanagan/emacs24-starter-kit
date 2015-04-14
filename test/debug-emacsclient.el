(with-current-buffer (get-buffer-create "*debug-emacsclient*")
  (pop-to-buffer (current-buffer))
  (erase-buffer)
  (require 'magit)
  (ignore-errors (magit-with-emacsclient 'pop-to-buffer)) ; start server
  (insert (format "system:\n")
          (format "  system-type:                   %s\n" system-type)
          (format "  system-configuration:          %s\n"
                  system-configuration)
          (format "  system-configuration-options:  %s\n"
                  system-configuration-options)
          (format "magit:        %s  (%s)\n"
          (locate-library "magit.el") (magit-version))
          (format "git:          %s  (%s)\n"
                  magit-git-executable
                  (nth 2 (split-string
                          (car (process-lines magit-git-executable
                                              "version")))))
          (format "emacs:        %s  (%s)\n"
                  (expand-file-name invocation-name invocation-directory)
                  emacs-version)
      "server:\n"
      (format "  server-running-p:  %s\n" (server-running-p))
      (format "  server-process:    %S\n" server-process)
      (format "  server-use-tcp:    %s\n" server-use-tcp)
      (format "  server-name:       %s\n" server-name))
  (insert (format "  server-socket-dir: %s\n" server-socket-dir))
  (when (file-directory-p server-socket-dir)
    (dolist (file (directory-files server-socket-dir nil "^[^.]"))
      (insert (format "    %s\n" file))))
  (insert (format "  server-auth-dir:   %s\n" server-auth-dir))
  (when (file-directory-p server-auth-dir)
    (dolist (file (directory-files server-auth-dir nil "^[^.]"))
      (insert (format "    %s\n" file))))
  (let* ((val magit-emacsclient-executable)
         (def (default-value 'magit-emacsclient-executable))
         (warning-minimum-level :error)
         (warning-minimum-log-level :error)
         (fun (magit-locate-emacsclient)))
    (insert "magit-emacsclient-executable:\n"
            (format "  value:   %s  (%s)\n" val
                    (and val (magit-emacsclient-version val)))
            (format "  default: %s  (%s)\n" def
                    (and def (magit-emacsclient-version def)))
            (format "  funcall: %s  (%s)\n" fun
                    (and fun (magit-emacsclient-version fun)))))
  (insert "path:\n"
          (format "  $PATH:     %S\n" (getenv "PATH"))
          (format "  exec-path: %s\n" exec-path)
          (format "  used path:\n"))
  (let ((path (cons (directory-file-name invocation-directory)
                    (cl-copy-list exec-path)))
        fixup)
    (when (eq system-type 'darwin)
      (setq fixup (expand-file-name "bin" invocation-directory))
      (when (file-directory-p fixup)
        (push fixup path))
      (when (string-match-p "Cellar" invocation-directory)
        (setq fixup (expand-file-name "../../../bin" invocation-directory))
        (when (file-directory-p fixup)
          (push fixup path))))
    (setq path (delete-dups path))
    (dolist (dir path)
      (insert (format "    %s (%s)\n" dir (car (file-attributes dir))))
      (when (file-directory-p dir)
        (dolist (exec (directory-files dir nil "emacsclient"))
          (insert (format "      %s (%s)\n" exec
                          (ignore-errors
                            (magit-emacsclient-version
                             (expand-file-name exec dir))))))))))
