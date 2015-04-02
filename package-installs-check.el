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

(require 'cl-lib)
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
    (cl-set-difference installed-packages expected-packages)))

;; (unexpected-packages lloyds-installed-packages)

(defun filter-alist-by-car (a-list list-of-cars)
  "Returns an alist consisting of all members of A-LIST whose first element is a member of LIST-OF-CARS."
  (if (member (caar a-list) list-of-cars)
      (cons (car a-list) (filter-alist-by-car (cdr a-list) list-of-cars))
    (if (null a-list) nil (filter-alist-by-car (cdr a-list) list-of-cars))))



(defun get-requirements-list ()
  "Get a nested list tructure containing requirements of installed packages in form (package-name (version-list))."
  ;;package-alist is an alist associating package names with
  ;; a cl-struct-package-desc structure
  (cl-remove-duplicates
   (cl-remove-if #'null
                 (mapcar #'car
                         (mapcar #'car
                                 (mapcar #'package-desc-reqs
                                         (mapcar #'cadr
                                                 ;; we only care about packages that are dependencies of members of lloyds-installed-packages
                                                 ;; or dependencies of those dependencies, which we don't get yet. yuck.
                                                 (filter-alist-by-car package-alist lloyds-installed-packages))))))))

(defun get-installed-packages-and-dependents ()
  "Returns the set of all installed packages, and all packages upon which they depend."
  (cl-remove-duplicates
   (append (mapcar #'car package-alist)
           (get-requirements-list))))

(defun extra-packages-installed (expected-list)
  "")

