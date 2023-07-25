#|
# Why I Like LISP

!!! summary  "TL;DR"
    LISP makes it crazy easy to write domain-specific language that let me express
    what I want, in the way that I want. Which is very useful (see examples, below). 

To some degree, it does not matter what language I code in.
This book is about some important higher-level ideas so the choice
of language could be viewed as 
irrelevant (a more implementation detail).

That said, someone always ask "why do you use LISP?"  So...

Alan Kay once famously said
"Lisp isn't a language, it's a building material". I  agree!
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
The doscussion got so toxic that the leader of the Python community,
Guido van Rossum, quit the Python project.
"Now that (walrus) is done, I don't ever want to have to fight so hard for a 
(chage)  and find that so many people despise my decisions.", he said.

To a LISPer, that whole debate is just insane.
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
And if you don't like my `aif` macro? Fine, just don't use it.

XXX make this example after moonwalking

I've got several other examples of 
how a little LISP making a useful change to a language.  
They all uses `defmacro` and if you need a little reminder on how that works:

- Most things is LISP are lists, even the code.
- Macros are functions (called at load time) that return lists which the LISP interprets as code.
- Macros are "coded" so much as they are "drawn". XXX

If you need the full details, and lots of good tutorial examples,
go see the [LISP cookbook on macros](https://lispcookbook.github.io/cl-cookbook/macros.html).

## asd a dsas 

Consider the task
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
Now the above  example becomes something much more palatable.

    (o *company* manager home address streetNum)

 nice macro adds a constructor `make-struct0` to a list of `defstruct`s. 
To do this, it rewrites the struct definition such that:    

- `(defstruct x slot1 slot2...)` 
- becomes `(defstruct (x (:constructor make-x0)) slot1 slot2...)`
|#
(defmacro add-make0 (&rest defstructs) 
  `(progn 
     ,@(loop for (_ it . slots) in defstructs collect
         `(defstruct (,it (:constructor ,(intern (format nil "MAKE-~a0" it)))) 
            ,@slots))))
#|
This allows for a simple instance creation. In the following 

- `(make-crew0)` and `(make-person0)` are the primitive constructors;
- while `(make-crew)` and `(make-person)`  enforce certain conventions at creation time
  (e.g., compute a person's `age` from their year of birth (`yob`)
  and the current year).

Here we go:

```lisp
(add-make0
  (defstruct person name age salary)
  (defstruct crew  persons))

(defun make-person (name yob role)
  (let ((this-year (sixth (multiple-value-list (get-decoded-time))))
        ((salaries '((pilot . 30054) (walker . 18622 ) (floater . 17147))))
    (make-person0 :name name 
                  :role (cdr (assoc role salaries))
                  :age (- this-year yob)))

(defun make-crew (crew)
  (make-crew0 :persons (loop for (name yob role) in crew collect 
                         (make-person name yob role))))

(make-crew '((neil 1930 pilot) (buzz 1930 walker) (mike 1930 floater)))
    
=> #S(CREW :PERSONS
      (#S(PERSON :NAME NEIL :AGE 93 :ROLE PILOT) 
       #S(PERSON :NAME BUZZ :AGE 93 :ROLE WALKER)
       #S(PERSON :NAME MIKE :AGE 93 :ROLE FLOATER)))
```

Another macro, that is useful for frequency counts, is `freq`. This one is a little tricky.
Say some sylmbolx conds :
|#
(defmacro freq (x lst &optional (init 0))      
  "frequency counts for small group of symbols (say, less than 50)"
  `(cdr (or (assoc ,x ,lst :test #'equal)
            (car (setf ,lst (cons (cons ,x ,init) ,lst))))))
#|
## "I don't like what you've done here"
Say you don't like the code I've got here. No drama.
We don't need
to go all walrus about it. Just delete my code and do whatever it is you
wanted to do.  And send me that revised code-- I'd really enjoy seeing how
you organize your code.

