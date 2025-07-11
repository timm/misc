; vim: set lispwords+=def,->,let+ :

(defmacro def (name args &body body)
  (let ((opts '()) (keys '()))
    (dolist (a args)
      (cond
        ;; Keyword arg: (:kw val) → (kw val)
        ((and (consp a) (keywordp (car a)))
         (let* ((kw (car a))
                (str (symbol-name kw))
                (sym (intern str))) ; (find-package :cl))))
           (push `(,sym ,(cadr a)) keys)))
        ;; Optional with default
        ((consp a) (push a opts))
        ;; Optional symbol
        (t (push a opts))))
    `(defun ,name
       (,@(when opts `(&optional ,@(nreverse opts)))
        ,@(when keys `(&key ,@(nreverse keys))))
       ,@body)))

(defmacro -> (args &body body)
  `(lambda ,args ,@body))

(defmacro ->> (x &rest forms)
  (reduce (lambda (acc f)
            (if (listp f)
                `(,(car f) ,@(cdr f) ,acc)
                `(,f ,acc)))
          forms :initial-value x))

(defmacro map+ (fn list)
  `(remove-if-not #'identity (mapcar ,fn ,list)))

(defmacro let+ (bindings &body body)
  (labels 
    ((expand (bs inner)
	     (if (null bs)
	       `(progn ,@inner)
	       (destructuring-bind (name expr) (first bs)
		 (if (and (consp expr) (eq (first expr) '->))
		   `(labels ((,name ,(second expr) ,@(cddr expr)))
		      ,(expand (rest bs) inner))
		   `(let ((,name ,expr))
			    ,(expand (rest bs) inner)))))))
    (expand bindings body)))

(defun run (fn)
  #-sbcl (funcall fn)
  #+sbcl (handler-case
	   (funcall fn)
	   (error (e) (format t "✖ ~A: ~A~%" (type-of e) e)
		  (sb-ext:exit :code 1))))

(def a ((a 1000) (:z (+ a 2)))
  (let+ ((a    z)
	 (b    (+ a 1))                         
	 (two  (-> (x) (* 2 x)))
	 (four (-> (x) (two (two x))))) ; sees double
    (list b (four a)))) ;; => (4 12)


(print (a))
