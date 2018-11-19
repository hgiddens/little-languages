(defpackage :little-schemer.ch10
  (:use :common-lisp)
  (:import-from :little-schemer.ch01 :atom?)
  (:import-from :little-schemer.ch04 :o+ :o* :pick)
  (:import-from :little-schemer.ch05 :equal?)
  (:import-from :little-schemer.ch07 :a-pair? :build :revpair))

(in-package :little-schemer.ch10)

(defun new-entry (a b)
  (build a b))

(defparameter *entries*
  (list
   (new-entry '(appetizer entrée beverage)
              '(paté boeuf vin))
   (new-entry '(appetizer entrée beverage)
              '(beer beer beer))
   (new-entry '(beverage dessert)
              '((food is) (number one with us)))))

(defun lookup-in-entry-help (name names values entry-f)
  (cond ((null names) (funcall entry-f name))
        ((equal? (car names) name) (car values))
        (t (lookup-in-entry-help name (cdr names) (cdr values) entry-f))))

(defun lookup-in-entry (name entry entry-f)
  (lookup-in-entry-help name (first entry) (second entry) entry-f))

(defparameter *table*
  (list (first *entries*)
        (third *entries*)))

(defun extend-table (entry table)
  (cons entry table))

(defun lookup-in-table (name table table-f)
  (cond ((null table) (funcall table-f name))
        (t (lookup-in-entry name (car table)
                            (lambda (name*)
                              (lookup-in-table name* (cdr table) table-f))))))

(defun expression-to-action (e)
  (cond ((or (null e) (atom? e)) (atom-to-action e))
        (t (list-to-action e))))

(defun *const (e table)
  (declare (ignore table))
  (cond ((numberp e) e)
        ((eq e t) t)
        ((eq e nil) nil)
        (t (build 'primitive e))))

(defun text-of (e)
  (second e))

(defun *quote (e table)
  (declare (ignore table))
  (text-of e))

(defun initial-table (name)
  (declare (ignore name)))

(defun *identifier (e table)
  (lookup-in-table e table #'initial-table))

(defun *lambda (e table)
  (build 'non-primitive (cons table (cdr e))))

(defun table-of (np)
  (first np))

(defun formals-of (np)
  (second np))

(defun body-of (np)
  (third np))

(defun else? (q)
  (eq 'else q))

(defun question-of (line)
  (first line))

(defun answer-of (line)
  (second line))

(defun evcon (lines table)
  (cond ((else? (question-of (car lines)))
         (meaning (answer-of (car lines)) table))

        ((meaning (question-of (car lines)) table)
         (meaning (answer-of (car lines)) table))

        (t (evcon (cdr lines) table))))

(defun cond-lines-of (e)
  (cdr e))

(defun *cond (e table)
  (evcon (cond-lines-of e) table))

(defun evlis (args table)
  (cond ((null args) nil)
        (t (cons (meaning (car args) table)
                 (evlis (cdr args) table)))))

(defun primitive? (f)
  (eq (car f) 'primitive))

(defun non-primitive? (f)
  (eq (car f) 'non-primitive))

(defun apply-primitive (name vals)
  (cond ((eq name 'cons) (cons (first vals) (second vals)))
        ((eq name 'car) (car (first vals)))
        ((eq name 'cdr) (cdr (first vals)))
        ((eq name 'null?) (null (first vals)))
        ((eq name 'eq?) (eq (first vals) (second vals)))
        ((eq name 'atom?) (atom? (first vals)))
        ((eq name 'zero?) (zerop (first vals)))
        ((eq name 'add1) (1+ (first vals)))
        ((eq name 'sub1) (1- (first vals)))
        ((eq name 'number?) (numberp (first vals)))))

(defun apply-closure (closure vals)
  (meaning (body-of closure)
           (extend-table (new-entry (formals-of closure) vals)
                         (table-of closure))))

(defun apply* (fun vals)
  (cond ((primitive? fun) (apply-primitive (second fun) vals))
        ((non-primitive? fun) (apply-closure (second fun) vals))))

(defun function-of (e)
  (car e))

(defun arguments-of (e)
  (cdr e))

(defun *application (e table)
  (apply* (meaning (function-of e) table)
          (evlis (arguments-of e) table)))

(defun atom-to-action (e)
  (cond ((numberp e) #'*const)
        ((eq e t) #'*const)
        ((eq e nil) #'*const) ; todo: replace with 'f?
        ((eq e 'cons) #'*const)
        ((eq e 'car) #'*const)
        ((eq e 'cdr) #'*const)
        ((eq e 'null?) #'*const)
        ((eq e 'eq?) #'*const)
        ((eq e 'atom?) #'*const)
        ((eq e 'zero?) #'*const)
        ((eq e 'add1) #'*const)
        ((eq e 'sub1) #'*const)
        ((eq e 'number?) #'*const)
        (t #'*identifier)))

(defun list-to-action (e)
  (cond ((atom? (car e)) (cond ((eq (car e) 'quote) #'*quote)
                               ((eq (car e) 'lambda) #'*lambda)
                               ((eq (car e) 'cond) #'*cond)
                               (t #'*application)))
        (t '*application)))

(defun value (e)
  (meaning e nil))

(defun meaning (e table)
  (funcall (expression-to-action e) e table))
