Module simp
===========

Simp

Usage:

:   simp.py \[options\]

Options:

:   -h Help. -v Verbose. \--seed=\<n\> Set random number seed \[default:
    1\]. -k=\<n\> Speed in knots \[default: 10\].

Functions
---------

[bore(z, all, e=1e-05)]{.title-ref} :

[cols(src)]{.title-ref} : Ignore columns if, on line one, the name
contains \'?\'.

[first(a)]{.title-ref} :

[last(a)]{.title-ref} :

[numbins(lst, x=0, y=-1, want=True, cohen=0.2, enough=0.2,
trivial=0.05)]{.title-ref} :

[opt(d)]{.title-ref} :

[pairs(lst)]{.title-ref} :

[place(runs, x)]{.title-ref} :

[random()]{.title-ref} : random() -\> x in the interval \[0, 1).

[rows(x=None, f=sys.stdin)]{.title-ref} : Read from stdio or file or
string or list. Kill whitespace or comments.

[same(x)]{.title-ref} :

[shuffle(a)]{.title-ref} :

[smo(tab, n1=10)]{.title-ref} :

[symbins(lst, x=0, y=-1, want=True, \*\_)]{.title-ref} :

Classes
-------

[Row(tab, cells)]{.title-ref} : Class that can pretty print.

> \#\#\# Ancestors (in MRO)
>
> -   simp.o
>
> \#\#\# Methods
>
> [better(i, j)]{.title-ref} :
>
> [status(i)]{.title-ref} :

[Tab(lst=\[\])]{.title-ref} : Class that can pretty print.

> \#\#\# Ancestors (in MRO)
>
> -   simp.o
>
> \#\#\# Class variables
>
> [ch]{.title-ref} :
>
> \#\#\# Methods
>
> [add(i, row)]{.title-ref} :
>
> [bins(i, want=None)]{.title-ref} :
>
> [header(i, lst)]{.title-ref} :
>
> [row(i, lst)]{.title-ref} :

[o(\*\*d)]{.title-ref} : Class that can pretty print.

> \#\#\# Descendants
>
> -   simp.Row
> -   simp.Tab
>
> \#\#\# Methods
>
> [get(i, x, default=0)]{.title-ref} :
>
> [inc(i, x)]{.title-ref} :
