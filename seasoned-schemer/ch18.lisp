(defpackage :seasoned-schemer.ch18
  (:use :common-lisp)
  (:import-from :little-schemer.ch01 :atom?))
(in-package :seasoned-schemer.ch18)

(defun bons (kar)
  (let (kdr)
    (lambda (selector)
      (funcall selector
               (lambda (x) (setf kdr x))
               kar
               kdr))))

(defun kar (bons)
  (flet ((selector (set kar kdr)
           (declare (ignore set kdr))
           kar))
    (funcall bons #'selector)))

(defun kdr (bons)
  (flet ((selector (set kar kdr)
           (declare (ignore set kar))
           kdr))
    (funcall bons #'selector)))

(defun set-kdr (bons x)
  (flet ((selector (set kar kdr)
           (declare (ignore kar kdr))
           (funcall set x)
           bons))
    (funcall bons #'selector)))

(defun kons (kar kdr)
  (let ((bons (bons kar)))
    (set-kdr bons kdr)))

(defvar konsc)
(defvar kounter)
(let ((n 0))
  (setf konsc (lambda (kar kdr)
                (incf n)
                (kons kar kdr)))
  (setf kounter (lambda () n)))

(defun lots (m)
  (cond ((zerop m) ())
        (t (kons 'egg (lots (1- m))))))

(defun lenkth (l)
  (cond ((null l) 0)
        (t (1+ (lenkth (kdr l))))))

(defun add-at-end (l)
  (cond ((null (kdr l)) (funcall konsc (kar l) (kons 'egg ())))
        (t (funcall konsc (kar l) (add-at-end (kdr l))))))

(defun add-at-end-too (l)
  (labels ((a (ls)
              (cond ((null (kdr ls)) (set-kdr ls (kons 'egg ())))
                    (t (a (kdr ls))))))
    (a l)
    l))

(defparameter dozen (lots 12))
(defparameter bakers-dozen (add-at-end dozen))
(defparameter bakers-dozen-too (add-at-end-too dozen))
(defparameter bakers-dozen-again (add-at-end dozen))

(defun eklist? (ls1 ls2)
  (cond ((null ls1) (null ls2))
        ((null ls2) nil)
        (t (and (eq (kar ls1) (kar ls2))
                (eklist? (kdr ls1) (kdr ls2))))))

(defun same? (c1 c2)
  (let ((t1 (kdr c1))
        (t2 (kdr c2)))
    (set-kdr c1 1)
    (set-kdr c2 2)
    (let ((v (= (kdr c1) (kdr c2))))
      (set-kdr c1 t1)
      (set-kdr c2 t2)
      v)))

(defun last-kons (ls)
  (cond ((null (kdr ls)) ls)
        (t (last-kons (kdr ls)))))

(defparameter long
  (let ((l (lots 12)))
    (set-kdr (last-kons l) l)))

(defun finite-lenkth (p)
  (catch 'infinite
    (labels ((c (p q)
               (cond ((same? p q) (throw 'infinite nil))
                     ((null q) nil)
                     ((null (kdr q)) 1)
                     (t (+ (c (sl p) (qk q)) 2))))
             (qk (x) (kdr (kdr x)))
             (sl (x) (kdr x)))
      (cond ((null p) 0)
            (t (1+ (c p (kdr p))))))))
