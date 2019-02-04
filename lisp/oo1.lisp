(defclass magic () ())

(defmacro isa (x parent &rest slots)
  "simpler creator for claseses. see example in thingeg.lisp"
  (labels ((uses  
	     (slot x form)
	     `(,slot
		:initarg  ,(intern (symbol-name slot) "KEYWORD")
		:initform ,form
		:accessor ,(intern (format nil "~a-~a" x slot)))))
    `(progn
       (defun ,x  (&rest inits)
	 (apply #'make-instance (cons ',x inits)))
       (defclass ,x (,parent)
	 ,(loop for (slot form) in slots collect (uses slot x form))))))

(defmethod slots ((x magic) &optional show)
  (labels 
    ((hide (y)
        (and (not show)
	   (and (symbolp y) 
		(equal (elt (symbol-name y) 0) #\_))))
     (slots1 (y) ; what are the slots of a class?
	     #+clisp (class-slots (class-of y))
	     #+sbcl  (sb-mop:class-slots (class-of y)))
     (name (y) ; what is a slot's name?
	   #+clisp (slot-definition-name y)
	   #+sbcl  (sb-mop:slot-definition-name y)))
    (remove-if #'hide 
	       (mapcar #'name 
		       (slots1 x)))))

(isa stuff magic (a 1) (b 2) (_cache "tim"))

(defmethod show ((x t)) (print (has x)))

;;; has
(defmethod has ((x t))    x)
(defmethod has ((x cons)) (mapcar #'has x))
(defmethod has ((x magic))  
  (cons `(ako ,(type-of x))
	(mapcar #'(lambda(slot)
		    `(,slot . ,(has (slot-value x slot))))
		(slots x))))
;;; visit
(defmethod visit ((x t) f) (funcall f x))
(defmethod visit ((x function) f) (funcall f (format nil "~a" x)))
(defmethod visit ((x cons) f) (mapcar (lambda(y) (visit y f))  x))
(defmethod visit ((x magic) f)  
  (mapcar #'(lambda(slot)
	      (visit (slot-value x slot) f))
	  (slots x t)))

(show `(1 2 #'_has "asdas" ,(make-instance 'stuff :b 20)))
(show `(1 2 #'_has "asdas" ,(stuff :b 20)))
(show `(1 2 #'_has "asdas" ,(stuff)))


(visit `(1 2 #'_has "asdas" ,(stuff :b 20)) #'print)

