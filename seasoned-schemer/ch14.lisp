(defpackage :seasoned-schemer.ch14
  (:use :common-lisp)
  (:import-from :little-schemer.ch01 :atom?)
  (:import-from :little-schemer.ch04 :pick)
  (:import-from :little-schemer.ch05 :eqlist?))
(in-package :seasoned-schemer.ch14)

(defun leftmost (l)
  (cond ((null l) ())
        ((atom? (car l)) (car l))
        (t (let ((leftmost-from-car (leftmost (car l))))
             (cond ((atom? leftmost-from-car) leftmost-from-car)
                   (t (leftmost (cdr l))))))))

(defun rember1* (a list)
  (labels
      ((r (l)
         (cond ((null l) ())
               ((atom? (car l))
                (cond ((eq (car l) a) (cdr l))
                      (t (cons (car l) (r (cdr l))))))
               (t (let ((from-left (r (car l))))
                    (cond ((eqlist? from-left (car l))
                           (cons (car l) (r (cdr l))))
                          (t (cons from-left (cdr l)))))))))
    (r list)))

(defun depth* (l)
  (cond ((null l) 1)
        ((atom? (car l)) (depth* (cdr l)))
        (t (let ((car-depth (1+ (depth* (car l))))
                 (cdr-depth (depth* (cdr l))))
             (max cdr-depth car-depth)))))

(defun scramble (list)
  (labels ((s (l a)
             (cond ((null l) nil)
                   (t (let ((rest (cons (car l) a)))
                        (cons (pick (car l) rest) (s (cdr l) rest)))))))
    (s list nil)))

(defun leftmost* (list)
  (labels ((out (a) (return-from leftmost* a))
           (lm (l)
             (cond ((null l) nil)
                   ((atom? (car l)) (out (car l)))
                   (t (progn
                        (lm (car l))
                        (lm (cdr l)))))))
    (lm list)))

(defun rember1** (a l)
  (labels ((rm (a l)
             (cond ((null l) (throw 'oh 'no))
                   ((atom? (car l)) (if (eq (car l) a)
                                        (cdr l)
                                        (cons (car l) (rm a (cdr l)))))
                   (t (let ((result (catch 'oh (rm a (car l)))))
                        (if (atom? result)
                            (cons (car l) (rm a (cdr l)))
                            (cons result (cdr l))))))))
    (let ((result (catch 'oh (rm a l))))
      (if (atom? result) l result))))
