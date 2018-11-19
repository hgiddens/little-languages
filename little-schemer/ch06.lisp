(defpackage :little-schemer.ch06
  (:use :common-lisp)
  (:import-from :little-schemer.ch01 :atom?)
  (:import-from :little-schemer.ch04 :o+ :o* :oexpt))

(in-package :little-schemer.ch06)

(defun numbered? (e)
  (cond ((atom? e)
         (numberp e))

        ((or (eq (car (cdr e)) '+)
             (eq (car (cdr e)) '*)
             (eq (car (cdr e)) '**))
         (and (numbered? (car e))
              (numbered? (car (cdr (cdr e))))))))

(defun value (e)
  (cond ((and (atom? e) (numberp e))
         e)

        ((eq (operator e) '+)
         (o+ (value (first-sub-expression e))
             (value (second-sub-expression e))))

        ((eq (operator e) '*)
         (o* (value (first-sub-expression e))
             (value (second-sub-expression e))))

        ((eq (operator e) '**)
         (oexpt (value (first-sub-expression e))
                (value (second-sub-expression e))))))

(defun first-sub-expression (e)
  (car (cdr e)))

(defun second-sub-expression (e)
  (car (cdr (cdr e))))

(defun operator (e)
  (car e))

(defun sero? (n)
  (null n))

(defun edd1 (n)
  (cons nil n))

(defun zub1 (n)
  (cdr n))

(defun s+ (a b)
  (cond ((sero? b) a)
        (t (edd1 (s+ a (zub1 b))))))

