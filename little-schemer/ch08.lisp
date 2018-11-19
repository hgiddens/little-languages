(defpackage :little-schemer.ch08
  (:use :common-lisp)
  (:import-from :little-schemer.ch01 :atom?)
  (:import-from :little-schemer.ch04 :o+ :o* :oexpt)
  (:import-from :little-schemer.ch05 :equal?)
  (:import-from :little-schemer.ch06 :first-sub-expression :operator :second-sub-expression))

(in-package :little-schemer.ch08)

(defun eq?-c (a)
  (lambda (x)
    (eq x a)))

(defparameter eq?-salad (eq?-c 'salad))

(defun rember-f (test?)
  (lambda (a l)
    (cond ((null l) nil)
          ((funcall test? (car l) a) (cdr l))
          (t (cons (car l) (funcall (rember-f test?) a (cdr l)))))))

(defun insertL-f (test?)
  (lambda (old new l)
    (cond ((null l) nil)
          ((funcall test? (car l) old)
           (cons new l))
          (t (cons (car l) (funcall (insertL-f test?) old new (cdr l)))))))

(defun insertR-f (test?)
  (lambda (old new l)
    (cond ((null l) nil)
          ((funcall test? (car l) old)
           (cons old (cons new (cdr l))))
          (t (cons (car l) (funcall (insertR-f test?) old new (cdr l)))))))

(defun insert-g (test? conser)
  (lambda (old new l)
    (cond ((null l) nil)
          ((funcall test? (car l) old)
           (funcall conser new old (cdr l)))
          (t (cons (car l) (funcall (insert-g test? conser) old new (cdr l)))))))

(defun seqL (new old l)
  (cons new (cons old l)))

(defun seqR (new old l)
  (cons old (cons new l)))

(defparameter insertL (insert-g #'eq (lambda (new old l)
                                       (cons new (cons old l)))))

(defun subst* (new old l)
  (declare (ignore old))
  (cons new l))

(defun segrem (new old l)
  (declare (ignore new old))
  l)

(defun yyy (a l)
  (funcall (insert-g #'eq #'segrem) a nil l))

(defun atom-to-function (x)
  (cond ((eq x '+) #'o+)
        ((eq x '*) #'o*)
        ((eq x '**) #'oexpt)))

(defun value (exp)
  (cond ((atom exp) exp)
        (t (funcall (atom-to-function (operator exp))
                    (value (first-sub-expression exp))
                    (value (second-sub-expression exp))))))

(defun multirember-f (test?)
  (lambda (a l)
    (cond ((null l) nil)
          ((funcall test? (car l) a)
           (funcall (multirember-f test?) a (cdr l)))
          (t (cons (car l) (funcall (multirember-f test?) a (cdr l)))))))

(defparameter multirember-eq (multirember-f #'eq))

(defun multiremberT (test? l)
  (cond ((null l) nil)
        ((funcall test? (car l)) (multiremberT test? (cdr l)))
        (t (cons (car l) (multiremberT test? (cdr l))))))

(defun multirember-co (a lat col)
  (cond ((null lat)
         (funcall col nil nil))

        ((eq (car lat) a)
         (multirember-co a (cdr lat) (lambda (newlat seen)
                                       (funcall col newlat (cons (car lat) seen)))))

        (t (multirember-co a (cdr lat) (lambda (newlat seen)
                                         (funcall col (cons (car lat) newlat) seen))))))

(defun a-friend (x y)
  (declare (ignore x))
  (null y))

(defun new-friend (newlat seen)
  (a-friend newlat (cons 'tuna seen)))

(defun latest-friend (newlat seen)
  (a-friend (cons 'and newlat) seen))

(defun multiinsertLR (new oldL oldR lat)
  (cond ((null lat) nil)

        ((eq (car lat) oldL)
         (cons new (cons oldL (multiinsertLR new oldL oldR (cdr lat)))))

        ((eq (car lat) oldR)
         (cons oldR (cons new (multiinsertLR new oldL oldR (cdr lat)))))

        (t (cons (car lat) (multiinsertLR new oldL oldR (cdr lat))))))

(defun multiinsertLR-co (new oldL oldR lat col)
  (cond ((null lat)
         (funcall col nil 0 0))

        ((eq (car lat) oldL)
         (multiinsertLR-co new oldL oldR (cdr lat)
                           (lambda (newlat seenL seenR)
                             (funcall col (cons new (cons oldL newlat)) (1+ seenL) seenR))))

        ((eq (car lat) oldR)
         (multiinsertLR-co new oldL oldR (cdr lat)
                           (lambda (newlat seenL seenR)
                             (funcall col (cons oldR (cons new newlat)) seenL (1+ seenR)))))

        (t
         (multiinsertLR-co new oldL oldR (cdr lat)
                           (lambda (newlat seenL seenR)
                             (funcall col (cons (car lat) newlat) seenL seenR))))))

(defun evens-only* (l)
  (cond ((null l) nil)

        ((atom? (car l))
         (cond ((evenp (car l)) (cons (car l) (evens-only* (cdr l))))
               (t (evens-only* (cdr l)))))

        (t (cons (evens-only* (car l))
                 (evens-only* (cdr l))))))

(defun evens-only*-co (l co)
  (cond ((null l)
         (funcall co nil 1 0))

        ((atom? (car l))
         (cond ((evenp (car l))
                (evens-only*-co (cdr l)
                                (lambda (new even odd)
                                  (funcall co (cons (car l) new) (* even (car l)) odd))))

               (t
                (evens-only*-co (cdr l)
                                (lambda (new even odd)
                                  (funcall co new even (+ odd (car l))))))))

        (t
         (evens-only*-co (car l)
                         (lambda (new even odd)
                           (evens-only*-co (cdr l)
                                           (lambda (new* even* odd*)
                                             (funcall co (cons new new*) (* even even*) (+ odd odd*)))))))))
