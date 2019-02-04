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

(defmethod slots ((x magic) &optional reveal-all)
  (labels 
    ((hide (y)
	   (and (not reveal-all)
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

#|
(macroexpand '(isa stuff magic (a 1 ) (b 2) (_cache "tim")))

(PROGN

  (DEFUN STUFF (&REST INITS)
	 (APPLY #'MAKE-INSTANCE (CONS 'STUFF INITS)))

  (DEFCLASS STUFF (MAGIC)
	    ((A :INITARG :A :INITFORM 1 :ACCESSOR STUFF-A)
	     (B :INITARG :B :INITFORM 2 :ACCESSOR STUFF-B)
	     (_CACHE :INITARG :_CACHE :INITFORM "tim" :ACCESSOR
		     STUFF-_CACHE)))) ;
|#

(defmethod show ((x t)) (print (has x)))

;--------- --------- --------- --------- --------- --------- ---------
;;; has
(defmethod has ((x t)) x)
(defmethod has ((x cons)) (mapcar #'has x))
(defmethod has ((x magic))  
  (cons `(ako ,(type-of x))
	(mapcar #'(lambda(slot)
		    `(,slot . ,(has (slot-value x slot))))
		(slots x))))

;--------- --------- --------- --------- --------- --------- ---------
;;; visit
(defmethod visit ((x t) f)
  (funcall f x ))

(defmethod visit ((x cons) f)
  (if (funp x)
    (funcall f (format nil "(~a)" (second x)))
    (mapcar (lambda(y) (visit y f)) x)))

(defmethod visit ((x magic) f)
  (mapcar (lambda(slot)
	      (visit (slot-value x slot) f))
	  (slots x)))

(defun funp(x)
   (and (consp x)
         (eql 2 (length x))
         (eql 'function (car x))))

;--------- --------- --------- --------- --------- --------- ---------
;;; walk
(defmethod walk ((x t) f &optional (depth 0)) 
  (funcall f x (* 2 depth)))

(defmethod walk ((x cons) f &optional (depth 0)) 
  (if (funp x)
    (funcall f (format nil "(~a)" (second x)) depth)
    (mapcar (lambda(y) (walk y f (1+ depth)))
	    x)))

(defmethod walk ((x magic) f &optional (depth 0))  
  (mapcar (lambda(slot)
	      (walk (slot-value x slot) f (1+ depth)))
	  (slots x t)))

;--------- --------- --------- --------- --------- --------- ---------
;;; demos
(defun chars (n &optional (string #\Space))
  (format nil "~V@{~a~:*~}" n string))

(defun line ()
  (format t "~&~a" (chars 50 #\-)))

(line)

(show `(1 2 #'_has "apples" ,(make-instance 'stuff :b 20)))
(show `(1 2 #'_has "apples" ,(stuff :b 20)))
(show `(1 2 #'_has "apples" ,(stuff)))

(line)

(visit `(1 2 #'_has "apples" ,(stuff :b 20)) #'print)

(line)

(walk  
     `(1 2 #'_has "apples" ,(stuff :b 20)) 
     (lambda (x pre)
         (format t "~&~a~a~%" (chars pre)  x)))
