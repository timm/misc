#!/usr/bin/env clisp
;  vim: set filetype=lisp tabstop=2 shiftwidth=2 expandtab :

(defun test1()
  (dolist (y '(1 2 3) x)
     (setf x (+ x y))))

(print (test1))

