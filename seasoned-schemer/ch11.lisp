(defpackage :seasoned-schemer.ch11
  (:use :common-lisp)
  (:import-from :little-schemer.ch04 :pick))
(in-package :seasoned-schemer.ch11)

(defun atom? (x)
  (not (listp x)))

(defun two-in-a-row-b? (preceding lat)
  (cond ((null lat) nil)
        (t (or (eq (car lat) preceding)
               (two-in-a-row-b? (car lat) (cdr lat))))))

(defun two-in-a-row (lat)
  (cond ((null lat) nil)
        (t (two-in-a-row-b? (car lat) (cdr lat)))))

(defun sum-of-prefixes-helper (l a)
  (cond ((null l) nil)
        (t (cons (+ (car l) a)
                 (sum-of-prefixes-helper (cdr l) (+ (car l) a))))))

(defun sum-of-prefixes (l)
  (sum-of-prefixes-helper l 0))

(defun scramble-helper (l acc)
  (cond ((null l) nil)
        (t (cons (pick (car l) (cons (car l) acc))
                 (scramble-helper (cdr l) (cons (car l) acc))))))

(defun scramble (l)
  (scramble-helper l nil))
