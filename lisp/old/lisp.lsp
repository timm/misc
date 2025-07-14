(defun head (x) (car x))
(defun tail (x) (cdr x))
(defun 2nd (x) (head (tail x)))
(defun 3rd (x) (head (tail (tail x))))

(defun ev (&key all env) 
  (labels ((ev1 (z)  
               (ev :all z :env env)))
             (print all) 
    (cond ((numberp all) all)
          ((symbolp all) (2nd (assoc all env)))
          (
            (and (print 1) (listp (head all))
                (eql (head (head all)) 'lambda))
            (eql (head (head all)) 'lambda)
           (ev :all (tail (tail (first all)))
              :env (match (first all) (rest all) env)))
          (t
            (apply 
                           (ev :all (head all) :env env)
                           (mapcar #'ev1 (tail all)))))))

(defun match (func args env)
 (labels ((two (a b) (list a b)))
    (append (mapcar #'two (2nd func) args) env)))

(trace ev)

(defun demo1()
  (ev
    :all 
    '(volume 3)
    ;'(+ pi (* r r))
    :env   
    '((volume (lambda (z) (* 1.33 pi z z z)))
      (pi  3.14) (r   2) (*   *) (+   +))))

(print (demo1))
