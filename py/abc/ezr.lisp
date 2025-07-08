(defpackage :my
  (:use :cl)
  (:shadow :map)) ; allow redefining `map` in this package

(in-package :my)

;; Redefine `map` as alias for `mapcar`
(defun map (&rest args) (remove-if #'null (apply #'mapcar args)))

(defmacro \ (params &rest body) `(lambda ,params ,@body))
(defmacro _! (x &rest ks) 
  (reduce (lambda (a k) `(getf ,a ',k)) ks :initial-value x))

(defmacro ! (x &rest ks)
  (if (listp x) `(_! ,x ,@ks) `(_! (getf it ',x) ,@ks)))

(set-dispatch-macro-character #\# #\?
  (lambda (s c n)
    `(slot-value self ',(read s))))

(print (funcall (\ (x) (* x x)) 5))
(print (map (\ (x) (* x x)) `(1 2 3 4)))

(defvar it '(seed 1234567891
              buckets 2
              p 2
              train "data/auto93.csv"
              bayes (m 1
                     k 1)
              about (what "mink.lisp"
                     why "optimizaiont"
                     when "(c) 2025"
                     how "MIT license"
                     who "Tim Menzies"
                     where "timm@ieee.org")))

(setf (! bayes m) 2332)
(print (! bayes m))
