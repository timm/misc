Module simp
===========
Simp

Usage:
    simp.py [options]

Options:
    -h          Help.
    -v          Verbose.
    --seed=<n>  Set random number seed [default: 1].
    -k=<n>      Speed in knots [default: 10].

Functions
---------

    
`bore(z, all, e=1e-05)`
:   

    
`cols(src)`
:   Ignore columns if, on line one, the name contains '?'.

    
`first(a)`
:   

    
`last(a)`
:   

    
`numbins(lst, x=0, y=-1, want=True, cohen=0.2, enough=0.2, trivial=0.05)`
:   

    
`opt(d)`
:   

    
`pairs(lst)`
:   

    
`place(runs, x)`
:   

    
`random()`
:   random() -> x in the interval [0, 1).

    
`rows(x=None, f=sys.stdin)`
:   Read from stdio or file or string or list. Kill whitespace or comments.

    
`same(x)`
:   

    
`shuffle(a)`
:   

    
`smo(tab, n1=10)`
:   

    
`symbins(lst, x=0, y=-1, want=True, *_)`
:   

Classes
-------

`Row(tab, cells)`
:   Class that can pretty print.

    ### Ancestors (in MRO)

    * simp.o

    ### Methods

    `better(i, j)`
    :

    `status(i)`
    :

`Tab(lst=[])`
:   Class that can pretty print.

    ### Ancestors (in MRO)

    * simp.o

    ### Class variables

    `ch`
    :

    ### Methods

    `add(i, row)`
    :

    `bins(i, want=None)`
    :

    `header(i, lst)`
    :

    `row(i, lst)`
    :

`o(**d)`
:   Class that can pretty print.

    ### Descendants

    * simp.Row
    * simp.Tab

    ### Methods

    `get(i, x, default=0)`
    :

    `inc(i, x)`
    :
