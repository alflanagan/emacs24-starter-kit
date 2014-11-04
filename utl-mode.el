;;; utl-mode.el --- major mode for editing TownNews UTL templates
;; Version: 

;; Copyright (C) 2003 Benj Carson, (C) 2014 A. Lloyd Flanagan

;; Maintainer: A. Lloyd Flanagan
;; Keywords: townnews cms blox languages templates
;; Created: 2003-08-23
;; Modified: 2014-11-04
;; X-URL:   none yet

(defconst utl-version "0.1.0"
  "UTL Mode version number.")

;;; License

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

;;; Usage

;; Rename this file to utl-mode.el if it isn't already then place it in
;; your Emacs lisp path (eg. site-lisp) and add to your .emacs file:
;;   (require 'utl-mode)

;; If you want colorization, turn on global-font-lock or
;; add this to your .emacs:
;;   (add-hook 'utl-mode-user-hook 'turn-on-font-lock)

;;; Commentary:

;; Utl-mode is for highlighting html templates written for the UTL
;; template engine (http://www.townnews.com).  This mode *does not*
;; inherit from html-mode since that mode seems like it might
;; interfere with what template designers might be trying to do.  This
;; mode makes an attempt to deliberately ignore html and highlight only
;; UTL code.

;; Since this is my first go at writing a major mode, this is pretty basic,
;; and may very well contain bugs.

;;; Changelog:

;; 0.1.0 Here goes nothing...  Note strings are not highlighted since I
;;       have yet to figure out how to get them highlighted only within
;;       { and }.

;;; Code:

;; mode-hook allows user to execute their own code when the mode is run,
;; mode-map allows user-defined keymaps
(require 'font-lock)
(require 'regexp-opt)

(defvar utl-mode-hook nil)
(defvar utl-mode-map nil
  "Keymap for UTL major mode")

;; Assign default keymap
(if utl-mode-map nil
  (setq utl-mode-map (make-keymap)))

;;;###autoload
(setq auto-mode-alist
	  (append
	   '(("\\.utl\\'" . utl-mode))
	   auto-mode-alist))

(defconst utl-functions
  (eval-when-compile
    (regexp-opt
     '(;; TODO: verify these, still utl-centric
       ;; standard built-in & custom functions (i.e. those listed in the docs)
       "capture" "config_load" "foreach" "foreachelse" "include" 
       "include_php" "insert" "if" "elseif" "else" "ldelim" "rdelim"
       "literal" "php" "section" "sectionelse" "strip" "assign" "counter"
       "cycle" "debug" "eval" "fetch" "html_checkboxes" "html_image"
       "html_option" "html_radios" "html_select_date" "html_select_time"
       "html_table" "math" "mailto" "popup_init" "popup" "textformat") t))
  "UTL built-in & custom functions.")

(defconst utl-constants
  (eval-when-compile
	(regexp-opt
	 '("TRUE" "FALSE" "NULL") t))
  "UTL constants.")
	   
	
(defconst utl-font-lock-keywords-1
  (list
   
   ;; Fontify built-in functions
   (cons
	(concat "\\<\\(" utl-functions "\\)\\>")
	'font-lock-keyword-face)

   (cons
	(concat "\\<\\(" utl-constants "\\)\\>")
	'font-lock-constant-face)

   )
  "Subdued level highlighting for UTL mode.") 

(defconst utl-font-lock-keywords-2
  (append
   utl-font-lock-keywords-1  
   (list

	;; Fontify variable names (\\sw\\|\\s_\\) matches any word character +
	;; underscore
	'("\\$\\(\\(?:\\sw\\|\\s_\\)+\\)" (1 font-lock-variable-name-face)) ; $variable
	'("->\\(\\(?:\\sw\\|\\s_\\)+\\)" (1 font-lock-variable-name-face t t)) ; ->variable
	'("\\.\\(\\(?:\\sw\\|\\s_\\)+\\)" (1 font-lock-variable-name-face t t)) ; .variable
	'("->\\(\\(?:\\sw\\|\\s_\\)+\\)\\s-*(" (1 font-lock-function-name-face t t)) ; ->function_call
	'("\\<\\(\\(?:\\sw\\|\\s_\\)+\\s-*\\)(" (1 font-lock-function-name-face)) ; word(
	'("\\<\\(\\(?:\\sw\\|\\s_\\)+\\s-*\\)[[]" (1 font-lock-variable-name-face)) ; word[
	'("\\<[0-9]+" . default)			; number (also matches word)

	;; Fontify strings
	;'("\"\\([^\"]\\)\"[^\"]+"
	;  (1 font-lock-string-face t t))
	))
  
   "Medium level highlighting for UTL mode.")

(defconst utl-font-lock-keywords-3
  (append
   utl-font-lock-keywords-2
   (list
	;; Fontify modifiers
	'("[^|]|\\[%1%\\]\\([^:|\]\n]+\\)"
	  (1 font-lock-function-name-face t t))
	;; Fontify config vars
	'("{\\(#\\(?:\\sw\\|\\s_\\)+#\\)}"
	  (1 font-lock-constant-face))))
  "Balls-out highlighting for UTL mode.")

(defvar utl-font-lock-keywords utl-font-lock-keywords-3
  "Default highlighting level for UTL mode")

;; Syntax table creation
(defvar utl-mode-syntax-table nil
  "Syntax table for utl-mode.")

(defun utl-create-syntax-table ()
  (if utl-mode-syntax-table
	  ()
	(setq utl-mode-syntax-table (make-syntax-table))

	; Add comment start & end ({* & *})
	(modify-syntax-entry ?{ "( 1" utl-mode-syntax-table)
	(modify-syntax-entry ?* ". 23" utl-mode-syntax-table)
	(modify-syntax-entry ?} ") 4" utl-mode-syntax-table)
	
	;; Make | a punctuation character
	(modify-syntax-entry ?| "." utl-mode-syntax-table)
	;; Make " a punctuation character so highlighing works withing html strings
	(modify-syntax-entry ?\" "." utl-mode-syntax-table)
	)
  (set-syntax-table utl-mode-syntax-table))

;;;###autoload
(defun utl-mode ()
  "Major mode for editing UTL template files"
  (interactive)
  (kill-all-local-variables)
  (utl-create-syntax-table)

  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults
        '((utl-font-lock-keywords)
          nil ; Keywords only (i.e. no comment or string highlighting
          t   ; case fold
          nil ; syntax-alist
          nil ; syntax-begin
          ))
  
  (setq font-lock-maximum-decoration t
		case-fold-search t)

  (setq major-mode 'utl-mode)
  (setq mode-name "utl")
  (run-hooks 'utl-mode-hook)
)

(provide 'utl-mode)
;;; utl-mode.el ends here
