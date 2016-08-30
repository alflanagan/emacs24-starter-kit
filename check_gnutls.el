(condition-case e
    (delete-process
     (gnutls-negotiate
      :process (open-network-stream "test" nil "www.google.com" 443)
      :hostname "www.google.com"
      :verify-error t))
  (error e))
