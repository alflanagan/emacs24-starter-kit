;;; package-installs-check.el --- check installed packages against expected list, with dependency checking -*- lexical-binding:t -*-

;; Copyright (C) 2015 A. Lloyd Flanagan

;; Author: A Lloyd Flanagan <a.lloyd.flanagan@gmail.com>
;; Created: 09 Mar 2015
;; Version: 0.1
;; Keywords: tools
;; Package-Requires: 

;; This file is NOT part of GNU Emacs.

;; package-installs-check is free software: you can redistribute it
;; and/or modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; package-installs-check is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied warranty
;; of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with package-installs-check.  If not, see
;; <http://www.gnu.org/licenses/>.

(eval-when-compile (require 'cl-lib))
(require 'package)

(defun extra-installed-packages ()
  "List packages which are installed (not including built-in)."
    (mapcar #'car package-alist))

(defun elpa-subdir (subdir-name)
  "Return SUBDIR-NAME if it is the name of an ELPA install directory, else nil."
  (if (member subdir-name '("." ".." "archives"))
      nil
    (let ((full-file-name (expand-file-name subdir-name (expand-file-name "elpa" user-emacs-directory))))
      (if (file-directory-p full-file-name) full-file-name nil))))

;; (elpa-subdir "archives")
;; (elpa-subdir "..")
;; (elpa-subdir "json-mode-20141105.138")
;; (elpa-subdir ".gitignore")


;; here's a simple difference between installed packages and packages
;; expected
;; PROBLEM: lists packages installed because they are dependency of an
;; installed package. We want to filter those
(defun unexpected-packages (expected-packages)
  "Return a list of installed packages which are not in EXPECTED-PACKAGES. Does not check dependencies, but should."
  (let ((installed-packages (mapcar #'car package-alist)))
    (set-difference installed-packages expected-packages)))

;; (unexpected-packages lloyds-installed-packages)

(defun get-requirements-list ()
  "Get a data structure containing requirements of installed packages"
  ;;package-alist is an alist associating package names with
  ;; a cl-struct-package-desc structure
  (mapcar #'package-desc-reqs (mapcar #'car (mapcar #'cdr package-alist))))
;; make list of all dependency specs -- will look like

(defun begins-with-atom (some-list)
  ""
  (atom-p (car some-list)))

(defun get-lists-starting-with-atoms(some-list)
  
  (if (atom (car some-list))
      (cons  some-list (get-lists-starting-with-atoms (cdr some-list)))
    (let ((car-lists (get-lists-starting-with-atoms (car some-list)))
          (cdr-lists (get-lists-starting-with-atoms (cdr some-list))))
      (if (not (null car-lists))
          (cons car-lists cdr-lists)
        cdr-lists))))

(get-lists-starting-with-atoms nil)
(get-lists-starting-with-atoms '(1 2 3))
(get-lists-starting-with-atoms '(nil (1 2 3)((3 4)(5 6))))
(nil (1 2 3) ((3 4) (5 6)))

(car-if-not-list nil)
(car-if-not-list '(nil nil nil ((dash (2 9 0)) (s (1 5 0))) ((font-utils (0 7 2))) (ucs-utils (0 8 0)) (list-utils (0 4 2)) (persistent-soft (0 8 10))))

;; (nil nil nil ((dash (2 9 0)) (s (1 5 0))) ((font-utils (0 7 2))
;; (ucs-utils (0 8 0)) (list-utils (0 4 2)) (persistent-soft (0 8 10))
;; (pcache (0 3 1))) nil ((persistent-soft (0 8 8)) (pcache (0 2 3))
;; (list-utils (0 4 2))) ((cl-lib (0 3)) (dash (2 10 0))) ((slime
;; (20100404))) ((cl-lib (0 5))) nil nil ...);; ((dash (2 9 0)) (s (1
;; 5 0))) ((font-utils (0 7 2)) (ucs-utils (0 8 0)) (list-utils (0 4
;; 2)) (persistent-soft (0 8 10)) (pcache (0 3 1))) nil
;; ((persistent-soft (0 8 8)) (pcache (0 2 3)) (list-utils (0 4 2)))
;; ((cl-lib (0 3)) (dash (2 10 0))) ((slime (20100404))) ((cl-lib (0
;; 5))) nil nil nil nil nil ((epl (0 4))) ((pcache (0 3 1))
;; (list-utils (0 4 2))) nil ((eieio (1 3))) ((emacs (24 4)) (dash (2
;; 6 0)) (cl-lib (0 5)) (json (1 3)) (let-alist (1 0 3))) nil nil ((s
;; (1 8 0)) (dash (2 4 0)) (f (0 14 0))) nil nil ((cl-lib (0 3))
;; (git-commit-mode (0 14 0)) (git-rebase-mode (0 14 0)))
;; ((coffee-mode (0 5 0))) nil nil nil ((emacs (24))) nil
;; ((json-reformat (20141009 1155)) (json-snatcher (20131110 1107)))
;; ((emacs (24 1)) (cl-lib (0 5))) nil nil nil nil nil nil nil
;; ((persistent-soft (0 8 8)) (pcache (0 2 3))) ((dash (2 4 0))
;; (pkg-info (0 4)) (let-alist (1 0 1)) (cl-lib (0 3)) (emacs (24 1)))
;; nil ((s (1 7 0)) (dash (2 2 0))) ((cl-lib (0 3))) ((company (0 8
;; 2)) (find-file-in-project (3 3)) (highlight-indentation (0 5 0))
;; (pyvenv (1 3)) (yasnippet (0 8 0))) nil ((dash (2 0 0)) (emacs
;; (24))) nil ((emacs (24 1)) (cl-lib (0 5))) ((emacs (24 1)) (cl-lib
;; (0 5))) ((coffee-mode (0 4 1))) ((emacs (24 1))))


