(defpackage :little-schemer.ch09
  (:use :common-lisp)
  (:import-from :little-schemer.ch01 :atom?)
  (:import-from :little-schemer.ch04 :o+ :o* :pick)
  (:import-from :little-schemer.ch05 :equal?)
  (:import-from :little-schemer.ch07 :a-pair? :build :revpair))

(in-package :little-schemer.ch09)

(defun keep-looking (a n lat)
  (cond
    ((numberp n)
     (keep-looking a (pick n lat) lat))

    (t
     (equal? n a))))

(defun looking (a lat)
  (keep-looking a (pick 1 lat) lat))

(defun shift (p)
  (build (first (first p))
         (build (second (first p))
                (second p))))

(defun align (pora)
  (cond ((atom? pora) pora)
        ((a-pair? (first pora)) (align (shift pora)))
        (t (build (first pora) (align (second pora))))))

(defun length* (pora)
  (cond ((atom? pora) 1)
        (t (o+ (length* (first pora))
               (length* (second pora))))))

(defun weight* (pora)
  (cond ((atom? pora) 1)
        (t (o+ (o* (weight* (first pora)) 2)
               (weight* (second pora))))))

(defun shuffle (pora)
  (cond ((atom? pora) pora)
        ((a-pair? (first pora)) (shuffle (revpair pora)))
        (t (build (first pora) (shuffle (second pora))))))

;; Y f = let a x = x x
;;           b x = f (x x)
;;       in a b
(defun Y (f)
  ((lambda (x) (funcall x x))
   ;; the (lambda (a) ...) here makes the recursion "lazy" and we otherwise blow the stack lol
   (lambda (x) (funcall f (lambda (a) (funcall (funcall x x) a))))))

(defparameter length**
  (Y (lambda (next)
       (lambda (l)
         (cond ((null l) 0)
               (t (1+ (funcall next (cdr l)))))))))
