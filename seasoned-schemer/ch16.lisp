(defpackage :seasoned-schemer.ch16
  (:use :common-lisp)
  (:import-from :little-schemer.ch01 :atom?))
(in-package :seasoned-schemer.ch16)

(defun sweet-tooth (food)
  (cons food (cons 'cake ())))

(defparameter last* 'angelfood)

(defun sweet-toothL (food)
  (setf last* food)
  (sweet-tooth food))

(defparameter ingredients ())
(defun sweet-toothR (food)
  (setf ingredients (cons food ingredients))
  (sweet-tooth food))

(defparameter deepM
  (let (numbers results)
    (lambda (m)
      (let ((result (find* m numbers results)))
        (if result
            result
            (setf result (deep m)
                  numbers (cons m numbers)
                  results (cons result results)))
        result))))

(defun deep (m)
  (cond ((zerop m) 'pizza)
        (t (cons (funcall deepM (1- m)) ()))))

(defparameter *deep-numbers* nil)
(defparameter *deep-results* nil)
(defun deepR (m)
  (let ((result (deep m)))
    (setf *deep-numbers* (cons m *deep-numbers*)
          *deep-results* (cons result *deep-results*))
    result))

(defun find* (n ns rs)
  (labels ((f (ns* rs*)
             (cond ((null ns*) nil)
                   ((= (car ns*) n) (car rs*))
                   (t (f (cdr ns*) (cdr rs*))))))
    (f ns rs)))

(defun length* (l)
  (cond ((null l) 0)
        (t (1+ (length* (cdr l))))))

(defun l (length)
  (lambda (l)
    (cond ((null l) 0)
          (t (1+ (funcall length (cdr l)))))))

(defparameter length*
  (let ((h (lambda (l)
             (declare (ignore l))
             0)))
    (setf h (l (lambda (x) (funcall h x))))
    h))

(defun y (l)
  (let (h)
    (setf h (funcall l (lambda (x) (funcall h x))))
    h))

(defparameter length* (y #'l))

(defun d (depth)
  (lambda (l)
    (cond ((null l) 1)
          ((atom? (car l)) (funcall depth (cdr l)))
          (t (let ((car-depth (1+ (funcall depth (car l))))
                   (cdr-depth (funcall depth (cdr l))))
               (max car-depth cdr-depth))))))

(defparameter depth* (y #'d))

(defparameter biz
  (let ((x 0))
    (lambda (f)
      (setf x (1+ x))
      (lambda (a)
        (if (= a x)
            0
            (funcall f a))))))
