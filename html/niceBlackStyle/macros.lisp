#|
## (Why (I (Love (LISP))))

**In brief:** Compared to other languages,
LISP offers fewer barriers and encourages
more experimentation.
Don't like something in the language? Then change it! E.g. see the macros
							    in this file.


[%autowidth,cols=">1,1,1",frame=ends,stripes=even]
|===
|`aif` 
|:
|(anaphoric if) for accessing a conditional without having to recompute it

|`o` 
|:
|easy  access to nested slots

|`defthing, defthings`  
|:
|fixes drawbacks with `defstruct` and OO in LISP  

|`freq` 
|:
|simplifying  symbol counting (for key sizes of 50 or less)
|===

### About LISP

[quote,Edsger Dijkstra, at his 1972 Turing Award lecture.]
LISP has jokingly been described as "the most intelligent way to misuse a computer". 
I think that description a great compliment because it transmits the full flavour of liberation: 
it has assisted a number of our most gifted fellow humans in thinking previously impossible thoughts.

LISP is the source.
LISP is a language that is over 60 years old yet still offers services
missing in more recent, supposedly better languages.
LISP is built to be adjustable, much more so than most  other languages.
As Alan Kay said
"Lisp isn't a language, it's a building material". 
Every part of LISP's read-eval-print loop can be customized. LISP's
macro system makes it trivial to extend the language. 
Any abstraction
you need inside the code can be made explicit and useful.
Why is that important? Well:

[quote, Paul Graham, from his book "ANSI Common LISP".]
The essence of writing reusable software is to separate the general
from the specific, and (LISP programming) inherently creates
such a separation.  Instead of devoting all your effort to writing
a single, monolithic application, you devote part of your effort
to building a language, and part to writing a (proportionately
smaller) application on top of it.

Not convinced?
Ok, then lets take a look at what happens in languages _without_ LISP's flexibility.

### Attack of the Walrus

Do you
recall the fight over 
the walrus operator (`:=`) in Python3? 
That operator 
allows assignments as part of expression evaluation. 
That way, if you need the result of a conditional, you do not have to run
that test again. For example, in Python:

```text
# without walrus
x := someBigLongCalculation()
if x: handle(x)

# with walrus
if x := someBigLongCalculation(): handle(x)
```

All in all it is a pretty minor addition to Python.
Even so, the walrus operator was hotly debated and
there were some very nasty social media posts
about the way the issue was decided.
The discussion got so toxic that the leader of the Python community,
Guido van Rossum, [quit the Python project](https://hub.packtpub.com/why-guido-van-rossum-quit/).
"Now that (walrus) is done, I don't ever want to have to fight so hard for a 
(chage)  and find that so many people despise my decisions.", he said.

To a LISPer, that whole debate is just insane.
If you want the walrus, it can be added with just two lines of code:
|#
(defmacro aif (test this &optional that)
   `(let ((it ,test)) (if it ,this ,that)))
#|
This code lets is trap the results of `test`  into `it`, then use `it` later; e.g.

    (aif (big-long-calculation)
      (foo it))

Note that this change can be made to your local
LISP without having to lobby some central committee. No drama.
And if you don't like tmy `aif` macro? Fine, just don't use it.

### Macro Basics

I've got several other examples of 
how a little LISP making a useful change to a language.  
They all uses `defmacro` and if you need a little reminder on how that works:

* Most things is LISP are lists, even the code.
* Macros are functions (called at load time) that return lists which the LISP interprets as code.
** So macros are code that rewrites code.
** In that code, the `&#96;backtick` defines a toggle enviornment where symbols are not evaluated,
   unless proceeded by a `,comma`. Also, the idiom `,@list` means create the list and lay it out flat.
* Macros are  not so much "coded" so much as they are "drawn". For example, the above `aif` definition,
  the last line shows the code that is desired.

If you need the full details, and lots of good tutorial examples,
go see the [LISP cookbook on macros](https://lispcookbook.github.io/cl-cookbook/macros.html).

And, just to repeat my main point, if you don't think the following are useful, then feel
free _not to use them_.

### Nested Slot Accessors

Consider  
nested accesses to a field inside a struct;  e.g. the `streetNum` of the the `address` of
the `home` of the `manager` of the `company. In standard LISP, that could be done wth:

    (slot-value 
       (slot-value 
          (slot-value 
             (slot-value *company 'manager) 'home) 'address) 'streetNum)

Pretty verbose, right? So lets fix that with a little macro.
This is a recursive macro (which is a little tricky) that works front to back over a list of slots. 
The first slot becomes the inner most accessor and accessors to the other slots are wrapped around it.
|#
(defmacro o (struct slot &rest slots) 
   (if slots
     `(o (slot-value ,struct ',slot) ,@slots)  ; case one: we have to recurse
     `(slot-value ,struct ',slot)))  ; case two: no slots left, so just do an access.
#|
With this macro, the above  example becomes something much more palatable.

    (o *company* manager home address streetNum)

One common idiom is to slip in a print statement to view the contents of a struct.
The following `oo` macro handles that (and not that it returns the struct so you can slip it in, get the print, and still
				 carry on processing the struct).
|#
(defmacro oo (struct slot &rest slots)
  `(progn (print (o ,struct ,slot ,@slots))
	  ,struct))
#|

### Saner, Simpler,  Objects

Like many people,  I have... issues... with the CLOS object system. 
It can be so verbose to (e.g.) define and new class, or specialize the initialization of  a new instance.
Worse, the functions that (e.g.) accesss the slot names of an instance vary from implementation to implementatin.

Hence I wrote `defthing` that adds a constructor to `defstruct` as well as  method `slots-of` that lists
all the slots of a thing.
|#
(defmacro defthing (it &rest has) 
  (labels ((make (x) (intern (format nil "%MAKE-~a" x))) 
           (name (x) (if (consp x) (car x) x))) 
    `(progn (defstruct (,it (:constructor ,(make it))) ,@has)
            (defmethod slots-of ((_ ,it)) ',(mapcar #'name has)))))
#|
Then, just cause it was so easy to do, I wrote `things` which turns
a list of `destructs` into  `defthings`:
|#
(defmacro things (&rest defstructs) 
  `(progn ,@(loop for (defstruct . slots) in defstructs collect `(defthing ,@slots))))
#|
This allows for simpler instance management. In the following, a set of structs are converted
to things (using `(things defstructs)`). 
Then we see (for example) the  `make-person` constructor 
 looking up our crew's salary and age before calling the constructor primitive constructor `%make-person`. 


```text
include::test-defthing.lisp[]
    
=> NEIL
```

Another macro, that is useful for frequency counts, is `freq`. This one is a little tricky.
Say some sylmbolx conds :
|#
; (defmacro freq (x lst &optional (init 0))      
;   "frequency counts for small group of symbols (say, less than 50)"
;   `(cdr (or (assoc ,x ,lst :test #'equal)
;             (car (setf ,lst (cons (cons ,x ,init) ,lst))))))
#|
### "I don't like what you've done here"
Say you don't like the code I've got here. No drama.
We don't need
to go all walrus about it. Just delete my code and do whatever it is you
wanted to do.  And send me that revised code-- I'd really enjoy seeing how
you organize your code.

[bibliography]
== References

* [[[DIJ72]]] Edsger W. Dijkstra (1972), The Humble Programmer (EWD 340) (ACM Turing Award lecture).
* [[[GRA95]]] Paul Graham (1995), ANSI Common Lisp.  Prentice-Hall
|#
