(defpackage :little-schemer.ch04
  (:use :common-lisp))

(in-package :little-schemer.ch04)

(defun o+ (a b)
  (cond ((zerop b) a)
        (t (1+ (o+ a (1- b))))))

(defun o- (a b)
  (cond ((zerop b) a)
        (t (1- (o- a (1- b))))))

(defun addtup (tup)
  (cond ((null tup) 0)
        (t (o+ (car tup) (addtup (cdr tup))))))

(defun o* (a b)
  (cond ((zerop b) 0)
        (t (o+ a (o* a (1- b))))))

(defun tup+ (tup1 tup2)
  (cond ((null tup1) tup2)
        ((null tup2) tup1)
        (t (cons (o+ (car tup1) (car tup2))
                 (tup+ (cdr tup1) (cdr tup2))))))

(defun o> (a b)
  (cond ((zerop a) nil)
        ((zerop b) t)
        (t (o> (1- a) (1- b)))))

(defun o< (a b)
  (cond ((zerop b) nil)
        ((zerop a) t)
        (t (o< (1- a) (1- b)))))

(defun o= (a b)
  (cond ((and (zerop a) (zerop b)) t)
        ((or (zerop a) (zerop b)) nil)
        (t (o= (1- a) (1- b)))))

(defun oexpt (a b)
  (cond ((zerop b) 1)
        (t (o* a (oexpt a (1- b))))))

(defun o/ (a b)
  (cond ((o< a b) 0)
        (t (1+ (o/ (o- a b) b)))))

(defun length* (l)
  (cond ((null l) 0)
        (t (1+ (length* (cdr l))))))

(defun pick (n l)
  (cond ((zerop (1- n)) (car l))
        (t (pick (1- n) (cdr l)))))

(defun rempick (n l)
  (cond ((zerop (1- n)) (cdr l))
        (t (cons (car l) (rempick (1- n) (cdr l))))))

(defun no-nums (l)
  (cond ((null l) l)
        ((numberp (car l)) (no-nums (cdr l)))
        (t (cons (car l) (no-nums (cdr l))))))

(defun all-nums (l)
  (cond ((null l) l)
        ((numberp (car l)) (cons (car l) (all-nums (cdr l))))
        (t (all-nums (cdr l)))))

(defun eqan? (a b)
  (cond ((and (numberp a) (numberp b))
         (o= a b))

        (t (eq a b))))

(defun occur (a l)
  (cond ((null l) 0)
        ((eqan? (car l) a) (1+ (occur a (cdr l))))
        (t (occur a (cdr l)))))

(defun one? (n)
  (o= 1 n))

(defun rempick* (n l)
  (cond ((one? n) (cdr l))
        (t (cons (car l) (rempick (1- n) (cdr l))))))
