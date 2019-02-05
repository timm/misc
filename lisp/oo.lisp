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


