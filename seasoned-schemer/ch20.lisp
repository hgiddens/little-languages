(defpackage :seasoned-schemer.ch20
  (:use :common-lisp)
  (:import-from :little-schemer.ch01 :atom?)
  (:shadow :car :cdr :cons))
(in-package :seasoned-schemer.ch20)

(defmacro define (name value)
  `(progn
     (defparameter ,name ,value)
     (when (functionp ,name)
       (setf (fdefinition (quote ,name))
             (lambda (&rest args) (apply ,name args))))))

(defmacro set! (a b)
  `(setq ,a ,b))

(defun car (x)
  (cl:car x))
(defun cdr (x)
  (cl:cdr x))
(defun cons (a d)
  (cl:cons a d)) 

(defparameter cons #'cons)
(defparameter car #'car)
(defparameter cdr #'cdr)
(defparameter atom? #'atom?)

(define add1
  (lambda (x)
    (1+ x)))

(define eq?
  (lambda (a b)
    (eq a b)))

(define null?
  (lambda (a)
    (null a)))

(define number?
  (lambda (a)
    (numberp a)))

(define sub1
  (lambda (a)
    (1- a)))

(define zero?
  (lambda (z)
    (zerop z)))

(define flog
  (lambda (m v)
    (format t "flog: ~A: ~A~%" m v)
    nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define text-of
  (lambda (x)
    (car (cdr x))))

(define formals-of
  (lambda (x)
    (car (cdr x))))

(define body-of
  (lambda (x)
    (cdr (cdr x))))

(define name-of
  (lambda (x)
    (car (cdr x))))

(define right-side-of
  (lambda (x)
    (cond ((null? (cdr (cdr x))) nil)
          (t (car (cdr (cdr x)))))))

(define cond-lines-of
  (lambda (x)
    (cdr x)))

(define question-of
  (lambda (x)
    (car x)))

(define answer-of
  (lambda (x)
    (car (cdr x))))

(define function-of
  (lambda (x)
    (car x)))

(define arguments-of
  (lambda (x)
    (cdr x)))

(define the-empty-table
  (lambda (name)
    (declare (ignore name))
    (flog (quote the-empty-table) name)))

(define lookup
  (lambda (table name)
    (funcall table name)))

(define extend
  (lambda (name1 value table)
    (lambda (name2)
      (cond ((eq? name2 name1) value)
            (t (funcall table name2))))))

(define define?
  (lambda (e)
    (cond ((atom? e) nil)
          ((atom? (car e)) (eq? (car e) (quote define)))
          (t nil))))

(define global-table the-empty-table)

(define box
  (lambda (it)
    (lambda (sel)
      (funcall sel it (lambda (new)
                        (set! it new))))))

(define setbox
  (lambda (b new)
    (funcall b (lambda (get set)
                 (declare (ignore get))
                 (funcall set new)))))

(define unbox
  (lambda (b)
    (funcall b (lambda (get set)
                 (declare (ignore set))
                 get))))

(define *define
  (lambda (e)
    (set! global-table (extend (name-of e)
                               (box (the-meaning (right-side-of e)))
                               global-table))))

(define lookup-in-global-table
  (lambda (name)
    (lookup global-table name)))

(define the-meaning
  (lambda (e)
    (meaning e lookup-in-global-table)))

(define meaning
  (lambda (e table)
    (funcall (expression-to-action e) e table)))

(define *quote
  (lambda (e table)
    (declare (ignore table))
    (text-of e)))

(define *identifier
  (lambda (e table)
    (unbox (lookup table e))))

(define *set
  (lambda (e table)
    (setbox (lookup table (name-of e))
            (meaning (right-side-of e) table))))

(define *lambda
  (lambda (e table)
    (lambda (args)
      (beglis (body-of e)
              (multi-extend (formals-of e)
                            (box-all args)
                            table)))))

(define beglis
  (lambda (es table)
    (cond ((null? (cdr es)) (meaning (car es) table))
          (((lambda (val)
              (declare (ignore val))    ; progn
              (beglis (cdr es) table))
            (meaning (car es) table))))))

(define box-all
  (lambda (vals)
    (cond ((null? vals) nil)
          (t (cons (box (car vals))
                   (box-all (cdr vals)))))))

(define multi-extend
  (lambda (ns vs table)
    (cond ((null? ns) table)
          (t (extend (car ns) (car vs) (multi-extend (cdr ns) (cdr vs) table))))))

(define *application
  (lambda (e table)
    (funcall (meaning (function-of e) table)
             (evlis (arguments-of e) table))))

(define *funcall
  (lambda (e table)
    (funcall (meaning (function-of (cdr e)) table)
             (evlis (arguments-of (cdr e)) table))))

(define *declare
  (lambda (e table)
    (declare (ignore e table))
    nil))

(define evlis
  (lambda (args table)
    (cond ((null? args) ())
          (t ((lambda (val)            ; enforce evaluation order of arguments
                (cons val (evlis (cdr args) table)))
              (meaning (car args) table))))))

(define a-prim
  (lambda (p)
    (lambda (arglist)
      (funcall p (car arglist)))))

(define b-prim
  (lambda (p)
    (lambda (arglist)
      (funcall p (car arglist) (car (cdr arglist))))))

(define *const
  ((lambda (*cons *car *cdr *null?
            *eq? *atom? *zero? *add1
            *sub1 *number? *flog)
     (lambda (e table)
       (declare (ignore table))
       (cond
         ((number? e) e)
         ((eq? e t) t)
         ((eq? e nil) nil)
         ((eq? e (quote cons)) *cons)
         ((eq? e (quote car)) *car)
         ((eq? e (quote cdr)) *cdr)
         ((eq? e (quote null?)) *null?)
         ((eq? e (quote eq?)) *eq?)
         ((eq? e (quote atom?)) *atom?)
         ((eq? e (quote zero?)) *zero?)
         ((eq? e (quote add1)) *add1)
         ((eq? e (quote sub1)) *sub1)
         ((eq? e (quote number?)) *number?)
         ((eq? e (quote flog)) *flog)
         (t (flog (quote const) e)))))
   (b-prim cons)
   (a-prim car)
   (a-prim cdr)
   (a-prim null?)

   (b-prim eq?)
   (a-prim atom?)
   (a-prim zero?)
   (a-prim add1)

   (a-prim sub1)
   (a-prim number?)
   (b-prim flog)))

(define *cond
  (lambda (e table)
    (evcon (cond-lines-of e) table)))

(define evcon
  (lambda (lines table)
    (cond ((null? lines)
           nil)

          ((meaning (question-of (car lines)) table)
           (meaning (answer-of (car lines)) table))

          (t
           (evcon (cdr lines) table)))))

;; not doing letcc because it's implemented in terms of letcc

(define value-of
  (lambda (e)
    (cond ((define? e) (*define e))
          (t (the-meaning e)))))

(define expression-to-action
  (lambda (e)
    (cond ((atom? e) (atom-to-action e))
          ((null? e) (atom-to-action e))
          (t (list-to-action e)))))

(define atom-to-action
  (lambda (e)
    (cond
      ((number? e) *const)
      ((eq? e t) *const)
      ((eq? e nil) *const)
      ((eq? e (quote cons)) *const)
      ((eq? e (quote car)) *const)
      ((eq? e (quote cdr)) *const)
      ((eq? e (quote null?)) *const)
      ((eq? e (quote eq?)) *const)
      ((eq? e (quote atom?)) *const)
      ((eq? e (quote zero?)) *const)
      ((eq? e (quote add1)) *const)
      ((eq? e (quote sub1)) *const)
      ((eq? e (quote number?)) *const)
      ((eq? e (quote flog)) *const)
      (t *identifier))))

(define list-to-action
  (lambda (e)
    (cond ((atom? (car e))
           (cond ((eq? (car e) (quote quote)) *quote)
                 ((eq? (car e) (quote lambda)) *lambda)
                 ((eq? (car e) (quote funcall)) *funcall)
                 ((eq? (car e) (quote declare)) *declare)
                 ((eq? (car e) (quote set!)) *set)
                 ((eq? (car e) (quote cond)) *cond)
                 (t *application)))

          (t *application))))
