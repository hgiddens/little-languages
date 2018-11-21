(defpackage :seasoned-schemer.ch19
  (:use :common-lisp)
  (:import-from :cl-cont :let/cc :with-call/cc)
  (:import-from :little-schemer.ch01 :atom?))
(in-package :seasoned-schemer.ch19)

(defun deep (m)
  (cond ((zerop m) 'pizza)
        (t (cons (deep (1- m)) ()))))

(defun four-layers (p)
  (setf p (cons p nil))
  (setf p (cons p nil))
  (setf p (cons p nil))
  (setf p (cons p nil)))

(defparameter toppings nil)

(defmacro letcc (k &body body)
  "LET/CC doesn't implicitly invoke K with the result."
  `(let/cc ,k
     (funcall ,k (progn ,@body))))

(with-call/cc
  (defun deepB (m)
    (cond ((zerop m)
           (letcc jump
             (setf toppings jump)
             'pizza))
          (t (cons (deepB (1- m)) ())))))

(defun deep&co (m k)
  (cond ((zerop m) (funcall k 'pizza))
        (t (deep&co (1- m) (lambda (x)
                             (funcall k (cons x ())))))))

(defun deep&coB (m k)
  (cond ((zerop m)
         (setf toppings k)
         (funcall k 'pizza))

        (t
         (deep&coB (1- m) (lambda (x) (funcall k (cons x ())))))))

(defparameter two-in-a-row?
  (labels ((w (a lat)
             (cond ((null lat) nil)
                   (t (let ((nxt (car lat)))
                        (or (eq nxt a)
                            (w nxt (cdr lat))))))))
    (lambda (lat)
      (cond ((null lat) nil)
            (t (w (car lat) (cdr lat)))))))

(defvar leave)

(defun walk (l)
  (cond ((null l) nil)
        ((atom? (car l)) (funcall leave (car l)))
        (t (progn (walk (car l))
                  (walk (cdr l))))))

(defun start-it (l)
  (catch 'start-it
    (setf leave (lambda (x) (throw 'start-it x)))
    (walk l)))

;; stupid lisp :(
