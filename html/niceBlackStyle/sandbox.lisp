(defmacro defthing (it &rest has) 
  "adds a contructor function symbol; adds a method to return slots in this struct"
  (labels ((make (x) (intern (format nil "MAKE-~a0" x))) ;create name for make primirive
           (name (x) (if (consp x) (car x) x))) ;slots with defaults are lists, else they are 1 word
    `(progn (defstruct (,it (:constructor ,(make it))) ,@has)
            (defmethod slots ((_ ,it)) ',(mapcar #'name has)))))

(defmacro defthings (&rest lst) 
  "swaps first symbol from 'defstruct' to 'defthing'"
  `(progn ,@(loop for (_ . slots) in lst collect `(defthing ,@slots))))
