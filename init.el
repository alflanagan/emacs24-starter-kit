;;; init.el --- Where all the magic begins  -*- lexical-binding: t; encoding: utf-8-unix; -*-
;;
;; Part of the Emacs Starter Kit
;;
;; This is the first thing to get loaded.
;;

(eval-when-compile (require 'package))

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;; Load CEDET.
;; See cedet/common/cedet.info for configuration details.
;; load from source when the CEDET_HOME environment variable is set
(let ((cedet-home (getenv "CEDET_HOME")))
  (when (not (null cedet-home))
    ;; IMPORTANT: You must place this *before* any CEDET component (including
    ;; EIEIO) gets activated by another package (Gnus, auth-source, ...).
    (load-file (expand-file-name "cedet-devel-load.el" cedet-home))
    (condition-case some-error
	(load-file (expand-file-name "cedet-contrib-load.el" (expand-file-name "contrib" cedet-home)))
      ((debug error) (message "[init] ERROR Probably need to re-compile contrib\ directory: %s" (cdr some-error))))))

(defun require-report-errors (feature &optional filename)
  "If FEATURE is not loaded, load it from FILENAME. If an error occurs, report and continue"
  (with-demoted-errors (concat "[init] Error loading " (symbol-name feature) ": %s")
    (require feature filename)))
               
;; Add further minor-modes to be enabled by semantic-mode.
;; See doc-string of `semantic-default-submodes' for other things
;; you can use here.
(add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode t)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-completions-mode t)
(if (fboundp 'global-cedet-m3-minor-mode)
    (add-to-list 'semantic-default-submodes 'global-cedet-m3-minor-mode t))

;; Enable Semantic
(semantic-mode 1)

;; Enable EDE (Project Management) features
;; (require 'ede/emacs)
;; (require 'ede/cpp-root)
;; (require 'ede/speedbar)
;; (require 'ede/linux)
;; (require 'ede/proj-elisp)

;; (global-ede-mode 1)
(if (fboundp 'semantic-load-enable-code-helpers)
    (semantic-load-enable-code-helpers)) ; Enable prototype help and smart completion 
(if (fboundp 'global-srecode-minor-mode)
    (global-srecode-minor-mode 1)) ; Enable template insertion menu

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
        (conversion-factor (* 1.618  (/ (float (frame-char-height)) (float (frame-char-width))))))
    (set-frame-size the-frame width (floor (/ width conversion-factor)))))

;; the same function except sets height > width
(defun set-portrait-size-by-golden-ratio (width &optional frame)
  "Set the width of the current frame to WIDTH (in characters) and the height to the reciprocal golden ratio."
  ;; our conversion factor is the golden ratio * the aspect ratio of a
  ;; character position
  ;;TODO: investigate get-frame-fringe, etc. for better values
  (let ((the-frame (first-non-null (list frame (selected-frame))))
        (conversion-factor (* 1.618  (/ (float (frame-char-width)) (float (frame-char-height))))))
    (set-frame-size the-frame width (floor (* width conversion-factor)))))

;; remember this directory
(defvar starter-kit-dir
  (file-name-directory (or load-file-name (buffer-file-name)))
  "Directory containing source for starter-kit package.")

;; load the starter kit from the `after-init-hook' so all packages are loaded
(add-hook 'after-init-hook
 `(lambda ()
    ;; only load org-mode later if we didn't load it just now
    ,(unless (and (getenv "ORG_HOME")
                (file-directory-p (expand-file-name "lisp"
                                                    (getenv "ORG_HOME"))))
       '(require 'org))
    ;; load up the starter kit
    (org-babel-load-file (expand-file-name "starter-kit.org" starter-kit-dir))))

;;; init.el ends here
