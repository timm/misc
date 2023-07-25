(load "macros")

(aif (member 'x '(x 1 y 2))
   (print (second it)))

(add-make0
  (defstruct person name age role)
  (defstruct crew  persons))

(defun make-person (name yob role)
  (make-person0 :name name 
                :role role
                :age (- (sixth (multiple-value-list (get-decoded-time))) yob)))

(defun make-crew (crew)
  (make-crew0 :persons (loop for (name yob role) in crew collect (make-person name yob role))))

(print 
(make-crew '((neil 1930 pilot) (buzz 1930 walker) (mike 1930 floater)))
)
