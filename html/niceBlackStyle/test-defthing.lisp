; test-defthings.lisp
(load "macros")
;-----------------------------------------------------------------------------------------------
(things
  (defstruct person name age salary)
  (defstruct crew  (size 0) persons))

(defun make-crew (crew)
  (%make-crew :persons (loop for (name yob role) in crew collect (make-person name yob role))))

(defun make-person (name yob role)
    (%make-person :name name :salary (role->salary role) :age  (- (this-year) yob)))
;-----------------------------------------------------------------------------------------------
(defun role->salary (role)
  (cdr (assoc role '((commander . 30054) (walker . 18622 ) (pilot . 17147)))))
           
(defun this-year ()
  (sixth (multiple-value-list (get-decoded-time))))
;-----------------------------------------------------------------------------------------------
(print  (make-crew '((neil 1930 commander) (buzz 1930 walker) (mike 1930 pilot))))
