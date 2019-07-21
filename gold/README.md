<img src="etc/img/banner.png">

<em>"Cause a little <strike>auk</strike> awk goes a long way"</em>


We like AWK. A lot. The simplicity and power of AWK often make it
just the right tool for the job.  But off-the-shelf AWK has no
support objects, inheritance, or polymorphism, unit tests, or
literate programming.  

Enter GOLD, which supports all the above.

<p align="center">
<a href="#install"><img src="etc/img/button_install.png"></a>
<a href="#tutorial"><img src="etc/img/button_tutorial.png"></a>
<a href="#tools"><img src="etc/img/button_tools.png"></a>
<a href="#tips"><img src="etc/img/button_tips.png"></a>
<a href="#contribute"><img src="etc/img/button_contribute.png"></a>
</p>

## Install

In a new directory, download a base system:

```bash
wget https://raw.githubusercontent.com/timm/misc/master/gold/79
chmod +x 79
./79 install
```

This generates the file `goldrc`. Optionally, you might want
to change this file's `Lib` variable (which 
stores
where GOLD generates its `.awk` files). 

```bash
if [ -z "$Gold" ]; then
export Gold="$(PWD)"
export Lib="$HOME/opt/gold"
export AWKPATH="$Lib:$AWKPATH"
export PATH="$Lib:$PATH"
fi
```

## Tutorial

In GOLD:

- objects constructors are  functions that start with one uppercase letter;
- methods are functions that have two uppercase letters: one for the class name and one for the method name. 
```awk
     1	function Col(i,pos,txt) {
     2	   isa(Object(i))
     3	   i.txt = txt
     4	   i.pos = pos="" ? 1 : pos
     5	}
     6	function Num(i) {
     7	  isa(Col(i))
     8	  i.hi = -1e32
     9	  i.lo =  1e32
    10	  i.n  = i.mu = i.m2 = i.sd = 0
    11	}
    12	function NumAdd(i,x,          delta) {
    13	  x    += 0
    14	  i.n  += 1
    15	  i.lo  = x < i.lo ? x : i.lo
    16	  i.hi  = x > i.hi ? x : i.hi
    17	  delta = x - i.mu
    18	  i.mu += delta/i.n
    19	  i.m2 += delta*(x - i.mu)
    20	  i.sd = (i.m2/(i.n-0.99999999))^0.5
    21	  return x
    22	}
    23	function Sym(i,pos,txt) {
    24	  isa(Col(i))
    25	  i.pos = pos
    26	  i.txt = txt
    27	  i.mode = ""
    28	  i.most = 0
    29	  i.ent =""
    30	  i.n = 0
    31	  i.ignore="?"
    32	  i.w=1
    33	  has(i,"counts")
    34	}
    35	function SymAdd(i,x,   tmp) {
    36	  if( x == i.ignore ) return x
    37	  i.n++
    38	  tmp = i.counts[x] = i.counts[x] + 1
    39	  if(tmp > i.mode) {
    40	    i.most = tmp
    41	    i.mode = x
    42	  }
    43	  return x
    44	}
```

```
## Tutorial 

### Source is kept in `.au` files

Source code is kept in `.au` files that the `auk` utility translates to `.awk` files:

    x.au -> ./auk -> x.awk

### Code is executed via the `./auk file` command

When  called with N arguments, `./auk args` converts all the .au files to .awk files in the current directory, 
then calls `gawk -f args`.

If called with no arguments, `./auk` just expands all the .au files.

### Dot Notation

GOLD expands dot notation; e.g. this code

```awk
function NumNorm(i,x) {
  return (x - i.lo)/(i.hi - i.lo + 1e-32)
}
```

expands to 

```awk
function NumNorm(i,x) {
  return (x - i["lo"])/(i["hi"] - i["lo"] + 1e-32)
}
```

Dots may be chained. e.g.
`a.b.c=1` becomes `a["b"]["c"]=1`.

- Method look up, for indirect functions starting with a capital letter. E.g. `@Add(i,10,20)`
  looks up the class of `i` and (sort of) replaces `@Add` with `ClassNameAdd`.

### Instance Creation

Constructor functions are (a) named lower case with one leading upper case letter;
  wraps the call to the superclass constructor  in `isa`; e.g.

```awk
function Num(i) {
  isa(Col(i))
  i.hi = -1e32
  i.lo =  1e32
  i.n  = i.mu = i.m2 = i.sd = 0
}
```

This expands to the following. Note now `isa` has extra arguments and the dots are expanded
to array field accessors.

```awk
function Num(i) {
  isa(i,"Num","Col",Col(i))
  i["hi"] = -1e32
  i["lo"] =  1e32
  i["n"]  = i["mu"] = i["m2"] = i["sd"] = 0
}
```

In the above example, `Num` is defined in terms of `Col`.  Now it turns out
tat `Col` is defined in terms of `Object`:

```Col
function Col(i,pos,txt) {
   Object(i)
   i.txt = txt
   i.pos = pos="" ? 1 : pos
}
```

The root constructor is `Object` which looks like this:

```
function Object(i) { 
  new(i) 
  i["isa"] = "Object"
  i["oid"] = ++OID 
}
function new(i) { split("",i,"") }
```
That is, `Objects` are arrays and newly created objects are initially empty.
After that, we add:

- a `oid` file holding a unique integer id and 
- an `isa` field describing their class. 

(Aside: note that the `isa` function always ensures that  the class `isa` field references the child, not parent class.
That is, everything is not an `Object`).
 


- `au`: an awk library for unittests, instance creation, method inheritance lookup

## Extras

The file `ell` and the directory `etc` are optional. They contain my environment tricks that you may not care about.  But if you want to try them:

```bash
sh ell
```

then <control-d> to exit.

For what details on what `ell` gives you, see etc/*.
