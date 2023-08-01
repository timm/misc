(defmacro defthing (it &rest has) 
  "adds a contructor function symbol; adds a method to return slots in this struct"
  (labels ((make (x) (intern (format nil "MAKE-~a0" x))) ;create name for make primirive
           (name (x) (if (consp x) (car x) x))) ;slots with defaults are lists, else they are 1 word
    `(progn (defstruct (,it (:constructor ,(make it))) ,@has)
            (defmethod slots ((_ ,it)) ',(mapcar #'name has)))))

(defmacro defthings (&rest lst) 
  "swaps first symbol from 'defstruct' to 'defthing'"
  `(progn ,@(loop for (_ . slots) in lst collect `(defthing ,@slots))))

(defmacro alambda (parms &body body)
   `(labels ((self ,parms ,@body))
      #'self))

 (alambda (n) ; factorial lambda
   (if (= n 0)
     1
     (* n (self (1- n)))))

(defun obj(&key (x 1) (y 2))
  (lambda (fun &optional one)
    (case fun
      (x?  x)
      (y?  y)
      (x! (setf x one))
      (y! (setf y  one)))))

(let ((x (obj :x 10)))
  (print (funcall x 'sum 100)))


