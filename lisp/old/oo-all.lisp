;  vim: set filetype=lisp tabstop=2 shiftwidth=2 expandtab :
(defun start (x)
  (if (consp x) (first x) x))

(defvar *meta* (make-hash-table))

(defstruct about has does )

(defmacro defklass (klass &key isa has does)
  "stuff"
  (let* ((b4      (and    isa  (gethash isa *meta*)))
         (has     (append has  (and b4 (about-has b4))))
         (does    (append does (and b4 (about-does b4))))
         (self    (gensym "SELF"))
         (message (gensym "MESSAGE")))
    (setf (gethash klass *meta*)
          (make-about :has has :does does))
    `(defun ,klass (&key ,@has) 
       (let ((,self (lambda (,message)
                      (case ,message
                        ,@(methods-as-case does)
                        ,@(datas-as-case (mapcar #'start has))))))
         (send ,self 'self! ,self)
         (send ,self 'isa! ',klass)
         ,self))))

(defun method-as-case (args)
  `(,(first args) 
    (lambda ,(second args) ,@(cddr args))))

(defun methods-as-case(xs) 
   (mapcar #'method-as-case xs))

(defun data-as-case (x) 
   `(,x (lambda () ,x)))

(defun datas-as-case (xs) 
   (mapcar #'data-as-case xs))

(defun xpand(x)
  (terpri) 
  (write 
    (macroexpand-1 x)
    :pretty t 
    :right-margin 40 
    :case :downcase)
  (terpri))

(defun send (obj mess &rest args) 
  (apply (funcall obj mess) args))

(let ((_counter 0))
  (defun counter () (incf _counter)))

; make id amd self and isa meta slots with a leading _

(defklass 
  thing 
  :has ((self)  (isa) (id (counter)))
  :does (
         (isa!  (x) (setf isa  x))
         (self! (x) (setf self x))
         (show () (dolist (slot (mapcar #'start (about-has (gethash isa *meta*))))
                    (if (not (member slot '(self isa)))
                       (print `(,slot . ,(send self slot)))
                    )))))
         ; (mapcar (lambda (slot) (print slot) (cons slot (send self slot))) (about-has (gethash isa *about*))))))

(thing)
(let ((x (thing))) (print (send x 'self)))
           
(print 2)

(defklass
  account
  :isa thing
  :has  ((name) (balance 0) (interest-rate .05))
  :does ((withdraw (amt)
                     (decf balance amt))
         (deposit (amt)
                  (incf balance amt))
         (interest ()
                   (incf balance
                         (* interest-rate balance)))))
                         
                         (account)

(send (account) 'show)

(xpand (gethash 'account *meta*))

(defklass
  trimmed-account
  :isa account
  :does ((withdraw (amt)
                   (if (<= amt balance)
                     (decf balance amt)
                     'insufficient-funds))))
(print 5)

(xpand (gethash 'trimmed-account *meta*))

(defun test1 ()
   (let ((acc (trimmed-account)))
      (print 10)
      (print (send acc 'deposit 100))
      (print (send acc 'interest))
      (print (send acc 'interest))
      (print (send acc 'balance))
      (print (send acc 'id))
      (dotimes (i 10)
         (print (send acc 'withdraw 20)))
      ))

(test1)

; polymorphism
(defklass 
  rectangle 
  :isa thing
  :has  ((x1 0) (y1 0) (x2 0) (y2 0))
  :does ((area () 
               (* (abs (- x1 x2)) (abs (- y1 y2))))))

; run it with an unknwon method
(defklass 
  circle
  :isa thing
  :has  ((x1 0) (y1 0) (radius  0))
  :does ((area () 
               (* pi radius radius))))

(defun test2()
  (let ((sum 0)
        (all (list (circle :radius 1) 
                   (rectangle :x2 10 :y2 10)
                   (circle :radius 2))))
    (dolist (one all)
      (incf sum (send one 'area)))
    (print sum)))

(test2)



