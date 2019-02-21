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

(defmacro defklass (klass &key isa has does)
  (let ((message (gensym "MESSAGE"))
        (parent  (gensym "PARENT")))
    `(defun ,klass (&key ,@has)
       (let ((,parent ,isa))
         (lambda (,message)
           (case ,message
             ,@(methods-as-case does)
             ,@(datas-as-case (mapcar #'start has))
             (otherwise 
                (and ,parent
                     (get-method ,parent ,message)))))))))

(defun method-as-case (args)
  `(,(first args) (lambda ,(second args) ,@(cddr args))))

(defun methods-as-case(xs)
  (mapcar #'method-as-case xs))

(defun data-as-case (x)
  `(,x (lambda () ,x)))

(defun datas-as-case (xs)
  (mapcar #'data-as-case xs))

(defun xpand(x)
  (write 
    (macroexpand-1 x)
    :pretty t :right-margin 50 :case :downcase)
  t)

(defun get-method (object message)
  "Return the method that implements message for this object."
  (funcall object message))

(defun send (object message &rest args)
  "Get the function to implement the message,
  and apply the function to the args."
  (apply (get-method object message) args))

(let ((counter 0))
  (defun id () (incf counter)))

(defklass thing :has ((id (id))))

; have to keep all in a cache and do an expand
; need a semd

(defklass
  account
  :isa (thing)
  :has  ((name) (balance 0) (interest-rate .05))
  :does ((withdraw (amt)
                     (decf balance amt))
         (deposit (amt)
                  (incf balance amt))
         (interest ()
                   (incf balance
                         (* interest-rate balance)))))

(defklass
  trimmed-account
  :isa (account)
  :does ((withdraw (amt)
                   (if (<= amt balance)
                     (decf balance amt)
                     'insufficient-funds))))

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

(one)
(one)

;;; ==============================

;;; ==============================

(defun withdraw (object &rest args)
  "Define withdraw as a generic function on objects."
  (apply (get-method object 'withdraw) args))


