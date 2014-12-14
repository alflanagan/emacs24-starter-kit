;;; init.el --- Where all the magic begins
;;
;; Part of the Emacs Starter Kit
;;
;; This is the first thing to get loaded.
;;

;; Load CEDET.
;; See cedet/common/cedet.info for configuration details.
;; load from source when the CEDET_HOME environment variable is set
(let ((cedet-home (getenv "CEDET_HOME")))
  (when (not (null cedet-home))
    ;; IMPORTANT: Tou must place this *before* any CEDET component (including
    ;; EIEIO) gets activated by another package (Gnus, auth-source, ...).
    (load-file (expand-file-name "cedet-devel-load.el" cedet-home))
    (load-file (expand-file-name "cedet-contrib-load.el" (expand-file-name "contrib" cedet-home)))))

(require 'package)
(add-to-list 'package-archives
             '("elpy" . "http://jorgenschaefer.github.io/packages/"))

;; Add further minor-modes to be enabled by semantic-mode.
;; See doc-string of `semantic-default-submodes' for other things
;; you can use here.
(add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode t)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-completions-mode t)
;; (if (boundp 'global-cedet-m3-minor-mode)
;;     (add-to-list 'semantic-default-submodes 'global-cedet-m3-minor-mode t))

;; Enable Semantic
(semantic-mode 1)

;; Enable EDE (Project Management) features
(global-ede-mode 1)

(declare-function -difference "dash.el" (list list2))
(declare-function semantic-add-system-include "semantic/dep.el" (DIR &optional MODE))

(add-hook 'after-init-hook
          (lambda ()
	    (let ((arduino-library-base "/usr/share/arduino/libraries"))
	      ;; don't set up arduino if arduino libraries not present
	      (when (file-directory-p arduino-library-base)
                (require 'dash)
                (mapc (lambda (dirname) (semantic-add-system-include dirname 'c++-mode))
		      (-difference  (directory-files arduino-library-base) '("." "..")))
		(semantic-add-system-include "/usr/share/arduino/hardware/arduino/cores/arduino" 'c++-mode)
		(semantic-add-system-include "/usr/share/arduino/hardware/arduino/variants/standard" 'c++-mode)))))

(defun require-report-errors (feature &optional filename)
 "If FEATURE is not loaded, load it from FILENAME. If an error occurs, report and continue"
(report-errors (concat (format "[init] Error loading %s: " (symbol-name feature))
                        "%s")
           (require feature filename)))
               
(require 'semantic/bovine/c)
(require 'semantic/bovine/gcc)
(require 'semantic/bovine/el)
(require-report-errors 'semantic/wisent/python)

;; load Org-mode from source when the ORG_HOME environment variable is set
(when (getenv "ORG_HOME")
  (let ((org-lisp-dir (expand-file-name "lisp" (getenv "ORG_HOME"))))
    (when (file-directory-p org-lisp-dir)
      (add-to-list 'load-path org-lisp-dir)
      (require 'org))))

(defun first-non-null (a-list &optional default)
  "Return first member of A-LIST which is not `nil.' If all are `nil', return DEFAULT if provided."
  (cond ((null a-list) default)
        ((null (car a-list)) (first-non-null (cdr a-list) default))
        (t  (car a-list))))

;; (first-non-null '(nil nil 3))
;; (first-non-null '(7 3 2 ))
;; (first-non-null nil)
;; (first-non-null '(nil nil nil) 7)

;; a function I find useful in site-specific settings
(defun set-frame-size-by-golden-ratio (width &optional frame)
  "Set the width of the current frame to WIDTH (in characters) and the height to the golden ratio."
  ;; our conversion factor is the golden ratio * the aspect ratio of a
  ;; character position
  ;;TODO: investigate get-frame-fringe, etc. for better values
  (let ((the-frame (first-non-null (list frame (selected-frame))))
        (conversion-factor (* 1.618  (/ (frame-char-height) (frame-char-width)))))
    (set-frame-size the-frame width (floor (/ width conversion-factor)))))

;; load the starter kit from the `after-init-hook' so all packages are loaded
(add-hook 'after-init-hook
 `(lambda ()
    ;; remember this directory
    (setq starter-kit-dir
          ,(file-name-directory (or load-file-name (buffer-file-name))))
    ;; only load org-mode later if we didn't load it just now
    ,(unless (and (getenv "ORG_HOME")
                  (file-directory-p (expand-file-name "lisp"
                                                      (getenv "ORG_HOME"))))
       '(require 'org))
    ;; load up the starter kit
    (org-babel-load-file (expand-file-name "starter-kit.org" starter-kit-dir))))

;;; init.el ends here
