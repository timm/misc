(defun head (x) (car x))
(defun tail (x) (cdr x))

(defun e (&key val env) 
  (labels ((e1 (z)  
               (e :val z :env env)))
    (cond ((numberp val) 
           val)
          ((symbolp val) 
           (tail (assoc val env)))
          ((listp val)
           (apply 
             (e :val (head val) :env env)
             (mapcar #'e1 (tail val)))))))

(trace e)

(defun demo1()
  (e 
    :val 
    '(+ pi (* r r))
    :env   
    '((pi . 3.14) (r . 2) (* . *) (+ . +))))

(demo1)
