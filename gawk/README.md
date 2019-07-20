# GOLD: the gawk object layer

The minimal system is two files: `auk` and `au`. 

## Install

- Place `auk` and `au` in a fresh directory. 
- If using Git: dd a `.gitignore` file in that directory

```
*.awk
```
## Language Guide

### Source is kept in `.au` files

Source code is kept in `.au` files that the `auk` utility translates to `.awk` files:

    x.au -> ./auk -> x.awk

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

This expands to the following (note now `isa` has extra arguments):

```awk
function Num(i) {
  isa(i,"Num","Col",Col(i))
  i["hi"] = -1e32
  i["lo"] =  1e32
  i["n"]  = i["mu"] = i["m2"] = i["sd"] = 0
}
```

In the baove example, `Num` is defined in terms of `Col`. `Col` is defined in terms of `Object`:

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
That is, `Objets have 
a `oid` file holding a unique integer id and 
an `isa` field describing their class. (Aside: note that the `isa` function resets the class `isa` field to the child class.)
 


- `au`: an awk library for unittests, instance creation, method inheritance lookup
