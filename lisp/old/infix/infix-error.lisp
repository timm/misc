;  vim: set filetype=lisp tabstop=2 shiftwidth=2 expandtab :

(define-condition malformed-infix-expression-error (error)
  ((text :initarg :text :reader malformed-infix-expression-error-text)
   (expression :initarg :expression :reader malformed-infix-expression-error-expression)))

;; from http://stackoverflow.com/a/7382977/1412255
(defmethod print-object ((err malformed-infix-expression-error) ostream)
  (print-unreadable-object (err ostream :type t)
    (format ostream "~s" (malformed-infix-expression-error-text err))
    (fresh-line ostream)
    (format ostream "Offending Expression: ~a" 
            (malformed-infix-expression-error-expression err))))

;; for error checking
(defun check-lst (lst ops)
  (if (evenp (length lst))
    (error 'malformed-infix-expression-error :text 
           "Expression has an even length" 
           :expression lst))
  (if (not (loop for i from 1 below (length lst) by 2
                 always (not (null (assoc (nth i lst) ops)))))
    (error 'malformed-infix-expression-error 
           :text "Not every element at odd index (even positions) in expression is a binary operator present in given *ops*"  
           :expression lst)))

(defun check-ops (ops)
  (if (not (and
             (listp ops)
             (not (null ops))
             (loop for item in ops
                   always (and (consp item) (symbolp (car item)) (numberp (cdr item))))))
    (error 'type-error :expected-type "non-empty-alist" :datum ops)))
