(defun head (x) (car x))
(defun tail (x) (cdr x))
(defun 2nd (x) (head (tail x)))

(defun ev (&key all env) 
  (labels ((ev1 (z)  
               (ev :all z :env env)))
    (cond ((numberp all) all)
          ((symbolp all) (2nd (assoc all env)))
          (t             (apply 
                           (ev :all (head all) :env env)
                           (mapcar #'ev1 (tail all)))))))

(trace ev)

(defun demo1()
  (ev
    :all 
    '(+ pi (* r r))
    :env   
    '((pi  3.14) (r   2) (*   *) (+   +))))

(print (demo1))
