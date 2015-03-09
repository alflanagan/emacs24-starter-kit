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

;; package-directory-list shows site/local elpa dirs, but not user's elpa
(mapcar #'directory-files package-directory-list)

(defun elpa-subdir (subdir-name)
  (if (member subdir-name '("." ".." "archives"))
      nil
    (let ((full-file-name (expand-file-name subdir-name (expand-file-name "elpa" user-emacs-directory))))
      (if (file-directory-p full-file-name) full-file-name nil))))


;; (elpa-subdir "archives")
;; (elpa-subdir "..")
;; (elpa-subdir "clojure-mode-20150305.715")
;; (elpa-subdir ".gitignore")

