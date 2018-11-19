(defpackage :little-schemer.ch01
  (:use :common-lisp))

(in-package :little-schemer.ch01)

;; CL defines ATOM which is similar, however:
;; > (atom ()) ; t
;; > (atom? ()) ; nil
(defun atom? (x)
  (not (listp x)))
