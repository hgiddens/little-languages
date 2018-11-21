(defpackage :seasoned-schemer.ch15
  (:use :common-lisp)
  (:import-from :seasoned-schemer.ch12 :member?))
(in-package :seasoned-schemer.ch15)

(defparameter x nil)
(defparameter food 'none)

(defun dinerR (food)
  (setf x food)
  (cons 'milkshake (cons food ())))

(defparameter omnivore
  (let (x)
    (lambda (food)
      (setf x food)
      (cons food (cons x ())))))

(defparameter gobbler
  (let ((x 'minestrone))
    (lambda (food)
      (setf x food)
      (cons food (cons x ())))))

(defun gourmand (food)
  (setf x food)
  (cons food (cons x ())))

(defun glutton (x)
  (setf food x)
  (cons 'more (cons x (cons 'more (cons x ())))))

(defun chez-nous ()
  (psetf food x
         x food))
