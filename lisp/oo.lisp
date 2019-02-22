;  vim: set filetype=lisp tabstop=2 shiftwidth=2 expandtab :
(defun now-account (name &optional (balance 0.00)
                    (interest-rate .06))
  "Create a new account that knows the following messages:"
  (lambda (message)
      (case message
        (withdraw (lambda (amt)
                      (if (<= amt balance)
                          (decf balance amt)
                          'insufficient-funds)))
        (deposit  (lambda (amt) (incf balance amt)))
        (balance  (lambda () balance))
        (name     (lambda () name))
        (interest (lambda ()
                      (incf balance
                            (* interest-rate balance)))))))

(defun start (x)
  (if (consp x) (first x) x))

(defvar *about* (make-hash-table))

(defstruct about has does)

(defmacro defklass (klass &key isa has does)
   (let* ((b4   (and isa (gethash isa *about*)))
          (has  (append  has  (and b4 (about-has b4))))
          (does (append  does (and b4 (about-does b4)))))
     (setf (gethash klass *about*)
           (make-about :has has
                       :does does))
     `(defklass1 ,klass ,has ,does)))

(defmacro defklass1 (klass has does)
  (let ((message (gensym "MESSAGE")))
    `(defun ,klass (&key ,@has)
       (lambda (,message)
         (case ,message
           ,@(methods-as-case does)
           ,@(datas-as-case (mapcar #'start has)))))))

(defun method-as-case (args)
  `(,(first args) (lambda ,(second args) ,@(cddr args))))

(defun methods-as-case(xs) (mapcar #'method-as-case xs))

(defun data-as-case (x) `(,x (lambda () ,x)))

(defun datas-as-case (xs) (mapcar #'data-as-case xs))

(defun xpand(x)
  (write (macroexpand-1 x)
    :pretty t :right-margin 50 :case :downcase)
  t)

(defun get-method (obj mess) (funcall obj mess))
(defun send (obj mess &rest args) (apply (get-method obj mess) args))

(let ((counter 0))
  (defun id () (incf counter)))

(defklass thing :has ((id (id))))

(print (gethash 'thing *about*))

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

(print (gethash 'account *about*))

(defklass
  trimmed-account
  :isa account
  :does ((withdraw (amt)
                   (if (<= amt balance)
                     (decf balance amt)
                     'insufficient-funds))))

(print (gethash 'trimmed-account *about*))
(defun one ()
   (let ((acc (trimmed-account)))
      (print (send acc 'deposit 100))
      (print (send acc 'interest))
      (print (send acc 'interest))
      (print (send acc 'balance))
      (print (send acc 'id))
      (dotimes (i 10)
         (print (send acc 'withdraw 20)))
      ))

(trimmed-account)


