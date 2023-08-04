(unless (fboundp 'srand) (load "lib"))

(defun failures () 
  (labels ((want (&aux (str (symbol-name sym)))
		 (if (fboundp sym) 
		   (equalp "fail-" (subseq str 0 (min 5 (length str))))))
	   (wants () (do-all-symbols (sym tmp)  (if (want sym) (push sym tmp))))
	   (run (fun (&aux (saved (copy-tree *settings*)))
		     (srand (? seed))
		     (prog1
		       (failed? fun)
		       (setf *settings* (copy-tree saved))))))
    (loop for sym in (sort (wants) #'string< :key #'symbol-name) 
	  sum (if (run sym) 1 0))))

(defun failed? (fun) 
  (ignore-errors (return-from failed? (funcall fun)))
  t)

(defun fail-error-no-crash  () (< 2 (/ 1 0)))
(defun fail-error-yes-crash  () (> 2 (/ 1 0)))

(defun fail-error-no  () (< 2 1))
(defun fail-error-yes  () (> 2 1))

(defun fail-errors () 
  (and (failed? #'fail-error-yes) (not (failed? #'fail-error-no)) i
       (failed? #'fail-error-yes-crash) (failed? #'fail-error-no-crash))) 

(print (fail-errors))

