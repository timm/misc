;  vim: set filetype=lisp tabstop=2 shiftwidth=2 expandtab :
(defun new-account (name &optional (balance 0.00)
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

(defmacro defklass (klass &key isa has does)
  (let* ((message (gensym))
         (locals  (mapcar #'start has))
         (inherited (if isa
                        (send isa '$vars)))
         (vars     (append locals inherited))) ; need to get the defaults into the has list
    `(defun ,klass (&key ,@has) ; does this work as .,has?
       (lambda (,message)
         (case ,message
           ,@(methods-as-case  does)
           ,@(datas-as-case     vars)
           (otherwise (get-method ,isa ,message)))))))

(defun method-as-case (args)
  `(,(first args) (lambda ,(second args) ,@(cddr args))))

(defun methods-as-case(lst)
  (mapcar #'method-as-case lst))

(defun start (x)
  (if (consp x) (first x) x))

(defun data-as-case (thing)
  `(,(start thing) (lambda () ,(start thing))))

(defun datas-as-case (lst isa)
  (let ((vars (append (mapcar #'start lst) 
                      (if isa 
                          (send isa '$locals)))))
    `(($locals (lambda () ',vars))
      ,@(mapcar  #'data-as-case vars))))

(defun xpand(x)
  (not 
    (write 
      (macroexpand-1 x)
      :pretty t :right-margin 50 :case :downcase)))

(defun one ()
  (xpand 
    '(defklass 
       new-account 
       :isa account
       :has ((name)  (balance 0) (interest-rate .05))
       :does ((withdraw (amt)
                        (if (<= amt balance)
                          (decf balance amt)
                          'insufficient-funds))
              (deposit (amt)
                       (incf balance amt))
              (interest ()
                        (incf balance
                              (* interest-rate balance)))))))


;;; ==============================

(defun get-method (object message)
  "Return the method that implements message for this object."
  (funcall object message))

(defun send (object message &rest args)
  "Get the function to implement the message,
  and apply the function to the args."
  (apply (get-method object message) args))

;;; ==============================

(defun withdraw (object &rest args)
  "Define withdraw as a generic function on objects."
  (apply (get-method object 'withdraw) args))


