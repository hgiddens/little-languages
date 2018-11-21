(defpackage :seasoned-schemer.ch13
  (:use :common-lisp)
  (:import-from :seasoned-schemer.ch12 :member?))
(in-package :seasoned-schemer.ch13)

(defun intersect (s1 s2)
  (labels ((i (s)
             (cond ((null s) nil)
                   ((member (car s) s2) (cons (car s) (i (cdr s))))
                   (t (i (cdr s))))))
    (i s1)))

(defun intersectall (lset)
  (labels ((intersect* (s1 s2)
             (labels ((intersect** (s)
                        (cond ((null s) nil)
                              ((member (car s) s2) (cons (car s) (intersect** (cdr s))))
                              (t (intersect** (cdr s))))))
               (cond ((null s2) (return-from intersectall nil))
                     (t (intersect** s1)))))
           (go* (l)
             (cond ((null (car l)) (return-from intersectall nil))
                   ((null (cdr l)) (car l))
                   (t (intersect* (car l) (go* (cdr l)))))))
    (cond ((null lset) nil)
          (t (go* lset)))))

(defun rember (a list)
  (labels ((r (l)
             (cond ((null l) ())
                   ((eq (car l) a) (cdr l))
                   (t (cons (car l) (r (cdr l)))))))
    (r list)))

(defun rember-beyond-first (a lat)
  (labels ((r (l)
             (cond ((null l) ())
                   ((eq (car l) a) ())
                   (t (cons (car l) (r (cdr l)))))))
    (r lat)))

(defun rember-upto-last (a lat)
  (labels ((r (l)
             (cond ((null l) ())
                   ((eq (car l) a)
                    (return-from rember-upto-last (r (cdr l))))
                   (t (cons (car l) (r (cdr l)))))))
    (r lat)))
