; vim: set filetype=lisp tabstop=2 shiftwidth=2 expandtab :

(defvar *ops0* '((if 1 if) (then 2 then) (and 3 and) 
                           (< 4 <) (> 4 >) (>= 4 >=) (<= 4 <=) (= 4 =) 
                           (+ 5 +) (- 5 -)
                           (* 6 *) (/ 6 /) 
                           ))

(defmacro while (test &body body)
  `(do () ((not ,test)) ,@body))

(defmacro xpand(x)
  `(progn 
     (terpri) 
     (write 
       (macroexpand-1 ',x) 
       :pretty t :right-margin 20 :case :downcase)
     (terpri)))

(defun postfix (lst &optional (ops *ops0*) st out)
  (labels
    ((up            ()  (let ((x (pop st))) (out+ x)))
     (st+           (x) (push   x st))
     (out+          (x) (push   (aka x) out))
     (aka           (x) (if     (operand x) x (third (operator x))))
     (prec          (x) (second (operator x)))
     (operand       (x) (not    (operator x)))
     (operator      (x) (assoc x ops))
     (moreImportant (x) (and st (<= (prec x) (prec (car st))))))
    (dolist (x lst)
      (cond ((operand x) (out+ x)) 
            (t           (while (moreImportant x) (up))
                         (st+ x))))
    (while st (up))
    (reverse out)))

(defun infix (lst &optional (ops *ops0*) tmp)
  (dolist (x lst (first tmp))
    (if (not (assoc x ops))
      (push x tmp)
      (let ((a (pop tmp))
            (b (pop tmp)))
        (push `(,x ,b ,a) tmp)))))

(defmacro $ (&rest lst)
  (infix (postfix lst)))

(xpand  ($ x = 2 if k > 10 and y = 10 then 1 + 3 - 4 / 10))

"
1.While there are input symbol left
…1.1 Read the next symbol from the input.
2.If the symbol is an operand
…2.1 Push it onto the stack.
3.Otherwise,
…3.1 the symbol is an operator.
…3.2 Pop the top 2 values from the stack.
…3.3 Put the operator, with the values as arguments and form a string.
…3.4 Push the resulted string back to stack.
4.If there is only one value in the stack
…4.1 That value in the stack is the desired infix string."

#|
If the incoming symbols is an operand,
print it..

If the incoming symbol is a left
parenthesis, push it on the stack.

If the incoming symbol is a right
parenthesis: discard the right
parenthesis, pop and print the stack
symbols until you see a left
parenthesis. Pop the left parenthesis
and discard it.

If the incoming symbol is an operator
and the stack is empty or contains a
left parenthesis on top, push the
incoming operator onto the stack.

If the incoming symbol is an operator
and has either higher precedence than
the operator on the top of the stack, or
has the same precedence as the operator
on the top of the stack and is right
associative -- push it on the stack.

If the incoming symbol is an operator
and has either lower precedence than the
operator on the top of the stack, or has
the same precedence as the operator on
the top of the stack and is left
associative -- continue to pop the stack
until this is not true. Then, push the
incoming operator.

At the end of the expression, pop and
print all operators on the stack. (No
parentheses should remain.)
|#
