(defpackage :little-schemer.ch03
  (:use :common-lisp))

(in-package :little-schemer.ch03)

(defun rember (a lat)
  (cond ((null lat) lat)
        ((eq (car lat) a) (cdr lat))
        (t (cons (car lat) (rember a (cdr lat))))))

(defun firsts (l)
  (cond ((null l) l)
        (t (cons (car (car l)) (firsts (cdr l))))))

(defun seconds (l)
  (cond ((null l) l)
        (t (cons (car (cdr (car l))) (seconds (cdr l))))))

(defun insertr (new old lat)
  (cond ((null lat) lat)
        ((eq (car lat) old) (cons old (cons new (cdr lat))))
        (t (cons (car lat) (insertr new old (cdr lat))))))

(defun insertl (new old lat)
  (cond ((null lat) lat)
        ((eq (car lat) old) (cons new lat))
        (t (cons (car lat) (insertl new old (cdr lat))))))

(defun subst* (new old lat)
  (cond ((null lat) lat)
        ((eq (car lat) old) (cons new (cdr lat)))
        (t (cons (car lat) (subst* new old (cdr lat))))))

(defun subst2 (new o1 o2 lat)
  (cond ((null lat) lat)
        ((or (eq (car lat) o1)
             (eq (car lat) o2))
         (cons new (cdr lat)))
        (t (cons (car lat) (subst2 new o1 o2 (cdr lat))))))

(defun multirember (a lat)
  (cond ((null lat) lat)
        ((eq (car lat) a) (multirember a (cdr lat)))
        (t (cons (car lat) (multirember a (cdr lat))))))

(defun multiinsertR (new old lat)
  ;; when we see old insert new to the right
  (cond ((null lat) lat)
        ((eq (car lat) old) (cons old (cons new (multiinsertR new old (cdr lat)))))
        (t (cons (car lat) (multiinsertR new old (cdr lat))))))

(defun multiinsertL (new old lat)
  (cond ((null lat) lat)
        ((eq (car lat) old) (cons new (cons old (multiinsertL new old (cdr lat)))))
        (t (cons (car lat) (multiinsertL new old (cdr lat))))))

(defun multisubst (new old lat)
  (cond ((null lat) lat)
        ((eq (car lat) old)
         (cons new (multisubst new old (cdr lat))))
        (t (cons (car lat) (multisubst new old (cdr lat))))))
