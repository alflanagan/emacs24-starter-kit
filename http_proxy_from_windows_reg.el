 (defun w32reg-read-key (key)
   "Read all values and subkeys for a key path in the Windows registry.
    The return value is a list (KEYNAME VALUES SUBKEYS).  KEYNAME is
    the name of the key. VALUES is a list of values, each one
    following this form: (NAME TYPE VALUE) where each are strings,
    and the TYPE is like \"REG_DWORD\" and so on.
    
    SUBKEYS is a simple list of strings.
    
    If the path does not exist, it returns nil.
    "
   (let ((reg.exe (concat (getenv "windir") "\\system32\\reg.exe"))
         keyname values subkeys (state 0))
    
     (with-temp-buffer
       (insert (shell-command-to-string
                (concat reg.exe " query " "\"" key "\"")))
    
       (while (not (= (point-min) (point-max)))
         (goto-char (point-min))
         (let ((start (point))
               (end (line-end-position))
               line this-value)
           (setq line (buffer-substring-no-properties start end))
           (delete-region start end)
           (delete-char 1) ;; NL
    
           (cond
            ((string/starts-with line "ERROR:")
             nil)
    
            ((string= "" line)
             (setq state (1+ state)))
    
            ((not keyname)
             (setq keyname line
                   state 1))
    
            ((eq state 1)
             (let ((parts (split-string line nil t)))
               (setq this-value (mapconcat 'identity (cddr parts) " "))
    
               ;; convert to integer, maybe
               (if (string= (nth 1 parts) "REG_DWORD")
                   (setq this-value
                         (string-to-number (substring this-value 2))))
    
               (setq values (cons (list (nth 0 parts)
                                        (nth 1 parts)
                                        this-value) values))))
            ((eq state 2)
             (setq subkeys (cons
                            (if (string/starts-with line keyname)
                                (substring line (1+ (length keyname)))
                              line)
                            subkeys)))
            (t nil)))))
    
     (and keyname
        (list keyname values subkeys))))

(defun w32reg-read-value (key value)
  "Read a value from a key location in the registry. The result
    is a list like (NAME TYPE VALUE), where the first two items are
    strings.  TYPE is like \"REG_DWORD\", \"REG_SZ\", \"REG_BINARY\",
    and so on.  If TYPE is \"REG_DWORD\", then the VALUE is a number.
    
    If the key value does not exist, it returns nil.
    "
  (let ((all (w32reg-read-key key))
        (c 0) L n r values)
    (and all
       (setq values (nth 1 all)
             L (length values))
       (while (and (not r) (< c L))
         (setq n (nth c values)
               c (1+ c))
         (if (string= value (car n))
             (setq r n))))
    r))


(defun w32reg-get-ie-proxy-config ()
  "Return the Proxy Server settings configured for IE, if enabled.
   The result is a list of cons cells; like this:
    
      ((\"http\" . \"127.0.0.1:8888\")
       (\"https\" .  \"127.0.0.1:8888\"))
    
   ...which is suitable for use with (setq url-proxy-services ...)
    
    "
  (let* ((rpath "HKCU\\Software\\Microsoft\\Windows\\CurrentVersion\\Internet Settings")
         (enabled (w32reg-read-value rpath "ProxyEnable"))
         r)
    (if (and enabled
           (numberp (nth 2 enabled))
           (>  (nth 2 enabled) 0))
        (let ((proxy (w32reg-read-value rpath "ProxyServer")))
          (mapcar `(lambda (elt)
                     (let ((x (split-string elt "=" t)))
                       (cons (car x) (cadr x))))
                  (split-string (nth 2 proxy) ";" t))))))
