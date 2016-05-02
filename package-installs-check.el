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
(defvar lloyds-installed-packages)

;; needed for batch mode.

(unless (boundp 'lloyds-installed-packages)
  (load-file "init.el")
  (run-hooks 'after-init-hook))

(declare-function -flatten "dash")


;; utility functions

(defun filter-alist-by-car (a-list list-of-cars)
  "Returns an alist consisting of all members of A-LIST whose first element is a member of LIST-OF-CARS."
  (if (member (caar a-list) list-of-cars)
      (cons (car a-list) (filter-alist-by-car (cdr a-list) list-of-cars))
    (if (null a-list) nil (filter-alist-by-car (cdr a-list) list-of-cars))))

(defun search-tree (some-car some-seq)
  "Recursively search for SOME-CAR as an element SOME-SEQ, which is treated as a tree."
  (if (null some-seq)
      nil
    (if (atom some-seq)
        (equal some-car some-seq)
      (or (search-tree some-car (car some-seq))
         (search-tree some-car (cdr some-seq))))))

;; (search-tree 'fred '(fred)) -> t
;; (search-tree 'fred '(alice fred)) -> t
;; (search-tree 'fred '((bob (carol (fred alice)) (wilma (betty barney))))) -> t
;; (search-tree 'fred '((bob (carol (ted alice)) (wilma (betty barney))))) -> nil

(defun digit-p (maybe-digit)
  "Returns t if MAYBE-DIGIT is a symbol or string consisting of a single digit 0-9."
  ;; FIXME this seems less than ideal
  (or (member maybe-digit '(0 1 2 3 4 5 6 7 8 9 ))   ;; actual digit
     (member maybe-digit (string-to-list "0123456789"))   ;; character
     (member (string-to-char maybe-digit) (string-to-list "0123456789"))))  ;;string



;; package-related functions

(defun package-get-deps (a-package)
  "Get a list of packages which are listed as dependencies of installed package A-PACKAGE."
  (let ((astruct (car (alist-get a-package package-alist))))
    (mapcar #'car (package-desc-reqs astruct))))

(defun get-requirements-list (expected-packages)
  "Get a nested list structure containing requirements of installed packages in list EXPECTED-PACKAGES in form (package-name (version-list))."
  ;;package-alist is an alist associating package names with
  ;; a cl-struct-package-desc structure
  (cl-remove-duplicates
   (cl-remove-if (lambda (x) (member x '(0 1 2 3 4 5 6 7 8 9)))
                 (-flatten
                  (mapcar #'package-desc-reqs
                          (mapcar #'cadr
                                  ;; we only care about packages that are dependencies of members of expected-packages
                                  ;; or dependencies of those dependencies, which we don't get yet. yuck. FIXME
                                  (filter-alist-by-car package-alist expected-packages)))))))

(defun get-packages-and-dependents (package-list)
  "Returns a set of packages in PACKAGE-LIST and any dependent packages found."
  (cl-remove-duplicates
   ;; recurse this adding found packages to package-list until no new
   ;; ones are found?
   (let ((requirements-list (get-requirements-list package-list)))
     (append package-list
             requirements-list
             ;; and get 1st-level dependencies of dependencies
             (get-requirements-list requirements-list)))))

(defun extra-packages-installed (expected-list)
  "Lists packages which are installed but not members of EXPECTED-LIST or a dependency thereof."
  (cl-set-difference (mapcar #'car package-alist) (get-packages-and-dependents expected-list)))

(defun build-expected-packages ()
  "Returns a list of packages expected to be present."
  ;; some packages are installed by starter-kit outside of
  ;; lloyds-installed-packages variable
  (append '(form-feed slime-repl slime) ;; starter-kit-lisp
          '(nodejs-repl nvm) ;; starter-kit-nodejs
          '(ctable concurrent)  ;; dependencies of ecb -- why not
   ;; found??
          '(ivy)  ;; find-file-in-project <- swiper <- ivy
          lloyds-installed-packages))

(defun append-string-to-buffer (string buffer)
  "Append STRING to the end of BUFFER."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-max))
      (insert string))))

(with-current-buffer "*scratch*"
  (save-excursion
    (goto-char (point-max))
    (print (extra-packages-installed (build-expected-packages)) (current-buffer))))



;; various debug code -- TOOD: make proper unit tests

;; (assert (member 'magit lloyds-installed-packages))
;; (assert (member 'magit (mapcar #'car  (filter-alist-by-car package-alist lloyds-installed-packages))))
;; ;; are second element of structure package-desc?
;; (assert (and (mapcar #'package-desc-p (mapcar #'cadr  (filter-alist-by-car package-alist lloyds-installed-packages)))))
;; ;; is magit's cl-struct-package-desc present
;; (assert (member 'magit (mapcar #'package-desc-name (mapcar #'cadr  (filter-alist-by-car package-alist lloyds-installed-packages)))))
;; ;; is git-commit-mode in one of the dependencies we pull?
;; (assert (search-tree 'git-commit-mode
;;                      (mapcar #'package-desc-reqs
;;                              (mapcar #'cadr
;;                                      (filter-alist-by-car package-alist lloyds-installed-packages)))))
;; (assert (member 'git-commit-mode (get-requirements-list lloyds-installed-packages)))
;; (assert (member 'git-commit-mode (get-packages-and-dependents lloyds-installed-packages)))
;; (assert (member 'git-rebase-mode (get-packages-and-dependents lloyds-installed-packages)))

;; FIXME -- yep, we don't handle dependencies of dependencies. epl is a dependency
;; of pkg-info, which is a dependency of flycheck
;; and pretty-symbols is from starter-kit-install-if-needed call in starter-kit-lisp
;;(extra-packages-installed lloyds-installed-packages)
;; ==> (darktooth-theme epl flymake-less form-feed list-utils nvm pretty-symbols ucs-utils)

;; next step -- create package buffer window allowing me to uninstall packages found?
;; modify lloyd.org to add packages?

