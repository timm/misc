

(defmacro my-if (x)
		 `(while x 1))

(defun xx () (/ a 1))

(defun run (fn)
  #-sbcl (funcall fn)
  #+sbcl (handler-case
	   (funcall fn)
	   (error (e) (format t "✖ ~A: ~A~%" (type-of e) e)
		  (sb-ext:exit :code 1))))

(run 'xx)
