;;; defmacro_exp.el --- my experiments with macros -*- lexical-binding: t; encoding: utf-8-unix; -*-
;;
;; 

(defmacro for (for-specs body)
  "Execute BODY zero to many times based on the values in FOR-SPECS.
FOR-SPECS should be a list (INIT-VALUE END-CHECK STEP-VALUE).
BODY is executed with anaphoric variable IT.
"
  (declare (indent 1))
  (let ((init-value (car for-specs))
         (end-check (cadr for-specs))
         (step-value (cl-caddr for-specs))
         (it (car for-specs)))
    `(while (not ,end-check)
       ,body
       ,step-value)
    it))
