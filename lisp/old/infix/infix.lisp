; vim: set filetype=lisp tabstop=2 shiftwidth=2 expandtab :

(load 'infix-error)

;;; from https://raw.githubusercontent.com/peey/ugly-tiny-infix-macro/master/ugly-tiny-infix-macro.lisp
(defparameter *ops*
  '((* . 5)
    (/ . 5)
    (mod . 5)
    (rem . 5)
    (+ . 6)
    (- . 6)
    (ash . 7) ; bitshift
    (< . 8)
    (<= . 8)
    (> . 8)
    (>= . 8)
    (= . 9)
    (/= . 9)
    (eq . 9) ; for checking boolean equality
    (eql . 9) ; more ways for checking equality and returning a boolean
    (equal . 9)
    (bit-and . 10)
    (bit-xor . 11)
    (bit-ior . 12) ; ior = inclusive or = same as cpp's bitwise or
    ;bit-nor and bit-nand are available but I'm unsure where to put them in this list
    (had . 20)
    (and . 31)
    (or . 32)
    (then  . 90)
    (if  . 100)
    ))

(defmacro xpand(x)
  `(progn 
     (terpri) 
     (write 
       (macroexpand-1 ',x) :miser-width 10
            :pretty t :right-margin 30 :case :downcase)
     (terpri)))

;; divide stack into things to do first (popped) and remaining stack
(defun recursively-pop-stack (stack element ops &optional (popped '()) )
  (if (and stack 
           (>=  (cdr (assoc element       ops)) 
                (cdr (assoc (first stack) ops))))
    (let ((first (pop stack)))
      ;; append to popped, not push, because order of returned 
      ;; popped stack should be same as was in working stack
      (recursively-pop-stack stack element ops (append popped (list first))))
    (values (push element stack) popped)))

;; apply popped operators to first two operands in q for each popped 
;; element (on top (index 0) of the stack should be the first operator to be applied)
(defun group (popped-stack q)
  ;; example operation:
  ;;input      : popped-stack  = (* -), q = (1 2 3)
  ;;iteration 1: popped-stack  = (-),   q = ((* 2 1) 3)
  ;;iteration 2: popped-stack  = (),    q = (- 3 (* 2 1))
  (loop for operator in popped-stack do
     (let ((a (pop q))
           (b (pop q)))
        (push `(,operator ,b ,a) q)))
  q)

;; an implementation of shunting-yard algorithm for operators w/o parenthesis for grouping
(defun shunting-yard (lst &optional (ops  *ops*))
  (let (q     ; output queue 
        stack ; temporary stack
        ) 
    (loop for element in lst do
          (if (assoc element ops)
            (multiple-value-bind (new-stack popped)
              (recursively-pop-stack stack element ops)
              (setf stack new-stack)
              (setf q (group popped q)))
            ;; if number / something that's expected to evaluated to a number
            (push element q)))
    ; append remaining stack to the q, get the single 
    ; expression left in the q of expressions
    (first (group stack q)))) 

(defmacro $ (&rest lst)
  "Infix binary operations for lisp!"
  (check-ops *ops*)
  (check-lst lst *ops*)
  (shunting-yard lst))

(xpand ($ emp = 23 had name = 'timm had age > 23 had salary < 23))
(xpand ($
  id = 31 
  if   emp = 23 had name = timm had age > 23 had salary < 23 
  then name = 22 and ll = 21 and kk = 2
))

;; need a parser that carries around a bindings list and returns bindings and flag t for success
