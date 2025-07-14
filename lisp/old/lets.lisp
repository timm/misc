(defmacro let+ (bindings &body body)
  "Handles mixed bindings, including variables, functions, and multiple-value-bind, preserving order."
  (let ((result `(progn ,@body)))
    (dolist (bind (reverse bindings))
      (let ((new-form
              (cond
                ;; Function binding
                ((and (listp bind) (>= (length bind) 3) (symbolp (car bind)) (listp (cadr bind)))
                 `(labels ((,(car bind) ,(cadr bind) ,@(cddr bind)))))
                ;; Multiple-value binding
                ((and (listp bind) (listp (car bind)))
                 `(multiple-value-bind ,(car bind) ,(cadr bind)))
                ;; Regular variable binding
                (t
                 `(let* (,bind))))))
        (setf result `(,@new-form ,result))))
    result))

(dolist( x '(
						  (let+ (
												((a b) (c 122))
												(_f1 (x) (* x 2))
												(c (_f1 21))
												((a b) (c 122))
												(_f2 (x) (* x 2))
												(c (_f2 21))
										) (print a))
            ))
	(terpri)
	(print (macroexpand x)))
