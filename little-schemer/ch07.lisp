(defpackage :little-schemer.ch07
  (:use :common-lisp)
  (:import-from :little-schemer.ch01 :atom?)
  (:import-from :little-schemer.ch02 :member?)
  (:import-from :little-schemer.ch03 :firsts :multirember :seconds))

(in-package :little-schemer.ch07)

(defun set? (l)
  (cond ((null l) t)
        ((member? (car l) (cdr l)) nil)
        (t (set? (cdr l)))))

(defun makeset (l)
  ;; (cond ((null l) l)
  ;;       ((member? (car l) (cdr l)) (makeset (cdr l)))
  ;;       (t (cons (car l) (makeset (cdr l)))))
  (cond ((null l) l)
        (t (cons (car l) (makeset (multirember (car l) (cdr l)))))))

(defun subset? (l1 l2)
  (cond ((null l1) t)
        (t (and (member? (car l1) l2)
                (subset? (cdr l1) l2)))))

(defun eqset? (s1 s2)
  (and (subset? s1 s2)
       (subset? s2 s1)))

(defun intersect? (s1 s2)
  (cond ((null s1) nil)
        (t (or (member? (car s1) s2)
               (intersect? (cdr s1) s2)))))

(defun intersect (s1 s2)
  (cond ((null s1) nil)

        ((member (car s1) s2)
         (cons (car s1) (intersect (cdr s1) s2)))
        (t (intersect (cdr s1) s2))))

(defun union* (s1 s2)
  (cond ((null s1) s2)

        ((member (car s1) s2)
         (union* (cdr s1) s2))

        (t (cons (car s1) (union* (cdr s1) s2)))))

(defun intersectall (l)
  (cond ((null (cdr l)) (car l))
        (t (intersect (car l) (intersectall (cdr l))))))

(defun a-pair? (l)
  (cond ((atom? l) nil)
        ((null (cdr l)) nil)
        ((null (cdr (cdr l))) t)
        (t nil)))

(defun first* (p)
  (car p))

(defun second* (p)
  (car (cdr p)))

(defun build (s1 s2)
  (cons s1 (cons s2 nil)))

(defun third* (l)
  (car (cdr (cdr l))))

(defun fun? (l)
  (set? (firsts l)))

(defun revpair (p)
  (build (second p) (first p)))

(defun revrel (l)
  (cond ((null l) l)
        (t (cons (revpair (car l))
                 (revrel (cdr l))))))

(defun fullfun? (l)
  (set? (seconds l)))

(defun one-to-one? (l)
  (and (fun? l) (fullfun? l)))
