#|
# Why I Like LISP

TL;DR:<em> LISP makes it easy to write domain-specific language that let me express
what I want, in the way that I want. Which is very useful (see examples, below).</em>

To some extant, the programming language used here is irrelevant, a more detail, that
is used to illustrate some ideas that are general across most langauges.

But someone always ask "why do you use LISP?"  so...

Alan Kay once famously said
"Lisp isn't a language, it's a building material". And I totally agree.
LISP is built to be flexible, much more so than most  other languages.
Why is such flexability important? Well, consider the fight over 
the walrus operator (`:=`) in Python3. 
The operator 
allowws assignments occur as part of the expression evaluation. 
That way, if you need the result of that conditional, you do not have to run
that test again. Fror example:

      # without walrus
      x := someBigLongCalculation()
      if x: handle(x)

      # with walrus
      if x := someBigLongCalculation(): handle(x)

All in all it is a pretty minor addition to the language. 
Even so, the operator was hotly debated and
there were some very nasty social media posts
about the way the issue was decided.
It got so nasty that the leader of the Python community,
Guido van Rossum, quit the Python project.
"Now that (walrus) is done, I don't ever want to have to fight so hard for a 
(chage)  and find that so many people despise my decisions.", he said.
I guess that's what happens when you work with a language that is so hard to change.

To a LISPer, that whoe debate is just insane.
If you want the walrus, it can be added with just two lines of code:
|#
(defmacro aif (test this &optional that)
  "when you want the result of a  conditional, bit you do not want to run that test again."
  `(let ((it ,test)) (if it ,this ,that)))
#|
This is called the _anaphoric if_ since anaphra is that part of a language that
lets you reference something mentioned in the past.
This code lets is trap the results of `test`  into `it`, then use it later; e.g.

    (aif (big-long-calculation)
      (foo it))

Note that this change can be made to your local
LISP without having to lobby some central committee. No drama.
And if you don't like walruses? Fine, just don't use it.

I've got several other examples of 
how a little LISP making a useful change to a language. Consider the task
of making nested accesses to a field inside a struct (e.g. the `streetNum` of the the `address` of
the `home` of the `manager` of the `company. In standard LISP, that could be done wth:

    (slot-value 
       (slot-value 
          (slot-value 
             (slot-value *company 'manager) 'home) 'address) 'streetNum)

Pretty verbose, right? So lets fix that with a little macro :
|#
(defmacro o (struct slot &rest slots) 
  "Nested slot accesros"
  (if slots
    `(o (slot-value ,struct ',slot) ,@slots)  ; case one: we have to recurse
    `(slot-value ,struct ',slot)))  ; case two: no slots left, so just do an access.
#|
Now the above  example becomes:

    (o *company* manager home address streetNum)

Another macro, that is useful for frequency counts, is `freq`. This one is a little tricky.
Say some sylmbolx conds :
|#
(defmacro freq (x lst &optional (init 0))      
  "frequency counts for small group of symbols (say, less than 50)"
  `(cdr (or (assoc ,x ,lst :test #'equal)
            (car (setf ,lst (cons (cons ,x ,init) ,lst))))))
|#
(defmacro my (&rest defstructs) 
  `(progn 
     ,@(loop for (_ it . slots) in defstructs collect
         `(defstruct (,it (:constructor ,(intern (format nil "MAKE-~a0" it)))) ,@slots))))


(my (defstruct a b c))

(defun make-a (b c)
  (make-a0 :b b :c c))

(print (make-a 1 2))