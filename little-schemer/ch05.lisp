(defpackage :little-schemer.ch05
  (:use :common-lisp)
  (:import-from :little-schemer.ch01 :atom?)
  (:import-from :little-schemer.ch04 :eqan? :o+))

(in-package :little-schemer.ch05)

(defun rember* (a l)
  (cond ((null l) l)

        ((and (atom? (car l)) (eq (car l) a))
         (rember* a (cdr l)))

        ((atom (car l))
         (cons (car l) (rember* a (cdr l))))

        (t (cons (rember* a (car l))
                 (rember* a (cdr l))))))

(defun insertR* (new old l)
  (cond ((null l) l)

        ((and (atom? (car l)) (eq (car l) old))
         (cons old (cons new (insertR* new old (cdr l)))))

        ((atom? (car l))
         (cons (car l) (insertR* new old (cdr l))))

        (t (cons (insertR* new old (car l))
                 (insertR* new old (cdr l))))))

(defun occur* (a l)
  (cond ((null l) 0)

        ((and (atom? (car l)) (eqan? (car l) a))
         (1+ (occur* a (cdr l))))

        ((atom? (car l))
         (occur* a (cdr l)))

        (t (o+ (occur* a (car l)) (occur* a (cdr l))))))

(defun subst** (new old l)
  (cond ((null l) l)

        ((and (atom? (car l)) (eqan? (car l) old))
         (cons new (subst** new old (cdr l))))

        ((atom? (car l))
         (cons (car l) (subst** new old (cdr l))))

        (t (cons (subst** new old (car l))
                 (subst** new old (cdr l))))))

(defun insertL* (new old l)
  (cond ((null l) l)

        ((and (atom? (car l)) (eqan? (car l) old))
         (cons new (cons old (insertL* new old (cdr l)))))

        ((atom? (car l))
         (cons (car l) (insertL* new old (cdr l))))

        (t (cons (insertL* new old (car l))
                 (insertL* new old (cdr l))))))

(defun member* (a l)
  (cond ((null l) nil)
        ((and (atom? (car l)) (eqan? (car l) a)) t)
        ((atom? (car l)) (member* a (cdr l)))
        (t (or (member* a (car l))
               (member* a (cdr l))))))

(defun leftmost (l)
  (cond ((atom? (car l)) (car l))
        (t (leftmost (car l)))))

(defun eqlist? (l1 l2)
  (cond
    ((and (null l1) (null l2)) t)
    ((or (null l1) (null l2)) nil)
    (t (and (equal? (car l1) (car l2))
            (equal? (cdr l1) (cdr l2))))))

(defun equal? (s1 s2)
  (cond ((and (atom? s1) (atom? s2)) (eqan? s1 s2))
        ((or (atom? s1) (atom? s2)) nil)
        (t (eqlist? s1 s2))))

(defun rember (a l)
  (cond ((null l) l)
        ((equal? (car l) a) (cdr l))
        (t (cons (car l) (rember a (cdr l))))))
