<img src="etc/img/banner.png">

<em>"Cause a little little <strike>auk</strike> awk goes a long way"</em>


We like AWK. A lot. The simplicity and power of AWK often make it just the right tool for the job.

Off-the-shelf AWK does not support objects, inheritance, or polymorphism, unit tests, or literate programming.  But a shown here, that's easy to add.

The result is _GOLD_, the GAWK object language. Share and enjoy.

<p align="center">
<a href="#install"><img src="etc/img/button_install.png"></a>
<a href="#tutorial"><img src="etc/img/button_tutorial.png"></a>
<a href="#tools"><img src="etc/img/button_tools.png"></a>
<a href="#tips"><img src="etc/img/button_tips.png"></a>
<a href="#contribute"><img src="etc/img/button_contribute.png"></a>
</p>

The minimal system is two files: `[auk](auk)` and `[au](au)`. 

## Install

- Place `auk` and `au` in a fresh directory. 
- Make `auk` executable (`chmod +x auk`).
- If using Git: dd a `.gitignore` file in that directory

```
*.awk
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
