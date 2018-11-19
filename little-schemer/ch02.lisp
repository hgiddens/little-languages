(defpackage :little-schemer.ch02
  (:use :common-lisp)
  (:import-from :little-schemer.ch01 :atom?))

(in-package :little-schemer.ch02)

(defun lat? (l)
  (cond ((null l) t)
        ((atom? (car l)) (lat? (cdr l)))
        (t nil)))

(defun member? (a lat)
  (cond ((null lat) nil)
        (t (or (eq (car lat) a)
               (member? a (cdr lat))))))
