;;   -*- lexical-binding: t; coding: utf-8-unix; -*-

(require 'cl-lib)

;; utility functions from Paul Graham's "On Lisp"
;; p. 45, figure 4.1
(cl-proclaim '(inline last1 single append1 conc1 mklist))

(defun last1 (lst)
  "Return final element of list LST."
  (car (last lst)))

(defun single (lst)
  "Predicate true if list LIST has a single element."
  (and (consp lst) (not (cdr lst))))

(defun append1 (lst obj)
  "Append element OBJ to the end of list LST."
  (append lst (list obj)))

(defun conc1 (lst obj)
  "Destructively add OBJ to end of list LST."
  (nconc lst (list obj)))

(defun mklist (obj)
  "Makes OBJ a list, unless it already is one."
  (if (listp obj) obj (list obj)))

;; p. 47, figure 4.3

(defun longer (x y)
  "Predicate true if list X is longer than list Y."
  (cl-labels ((compare (x y)
             (and (consp x)
                  (or (null y)
                      (compare (cdr x) (cdr y))))))
    (if (and (listp x) (listp y))
        (compare x y)
        (> (length x) (length y)))))

(defun filter (fn lst)
  "Apply function FN to each element of list LST, return list of non-nil results."
  (let ((acc nil))
    (dolist (x lst)
      (let ((val (funcall fn x)))
        (if val (push val acc))))
    (nreverse acc)))

(defun group (source n)
  "Return a list composed of the elements of list SOURCE, divided into lists of N elements each.

The final group will have less than N elements if N doesn't evenly divide (length SOURCE)."
  ;; there's a simpler non-tail-recursive version.
  (if (zerop n) (error "zero length"))
  (cl-labels ((rec (source acc)
             (let ((rest (nthcdr n source)))
               (if (consp rest)
                   (rec rest (cons (cl-subseq source 0 n) acc))
                   (nreverse (cons source acc))))))
    (if source (rec source nil) nil)))

(defun flatten (x)
  "Return a list composed of all the leaves of tree X."
  (cl-labels ((rec (x acc)
                   (cond ((null x) acc)
                         ((atom x) (cons x acc))
                         (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))

(defun prune (test tree)
  "Removes all items satisfying TEST in TREE, recursing over subtrees.

Differs from `cl-remove-if' as that function does not affect subtrees."
  (cl-labels ((rec (tree acc)
                   (cond ((null tree) (nreverse acc))
                         ((consp (car tree))
                          (rec (cdr tree)
                               (cons (rec (car tree) nil) acc)))
                         (t (rec (cdr tree)
                                 (if (funcall test (car tree))
                                     acc
                                   (cons (car tree) acc)))))))
    (rec tree nil)))

(cl-defun before (x y lst &key (test #'eql))
  "Predicate is true if X occurs before Y in list LST (using equality test TEST).

Attempts to return the cdr beginning with X. Will return as true
if X is present in LST and Y is not."
  (and lst
       (let ((first (car lst)))
         (cond ((funcall test y first) nil)
               ((funcall test x first) lst)
               (t (before x y (cdr lst) :test test))))))

(cl-defun after (x y lst &key (test #'eql))
  "Predicate is true if X occurs before Y in list
  LST (keyword :test specifies equality test TEST)

Attempts to return the cdr beginning with X. Will return nil if X
is present in LST and Y is not (unlike `before')."
  (let ((rest (before y x lst :test test)))
    (and rest (cl-member x rest :test test))))

(defun find2 (fn lst)
  (if (null lst)
      nil
    (let ((val (funcall fn (car lst))))
      (if val
          (cl-values (car lst) val)
        (find2 fn (cdr lst))))))
