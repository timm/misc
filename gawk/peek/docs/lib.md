#  lib.gold
  - [OO Support](#oosupport) : initialize an empty list
  - [Testing](#testing) : Print local variables, escaped from functions
  - [Maths](#maths) : Return absolute value of `x`.
  - [Printing](#printing) : Simple array printing.
  - [Reading](#reading) : Loop over a csv file `f`, setting the array `a` to the next record.

## OO Support
- i : untyped
===### List??
- i : untyped

<ul><details><summary><tt>List()</tt></summary>

```awk
function List(i) { split("",i,"") }
```

</details></ul>


 Initialize a new object, give it a unique id (in `i.id`)
- i : untyped
===### Object??
- i : untyped

<ul><details><summary><tt>Object()</tt></summary>

```awk
function Object(i) { List(i); i.id = ++Gold.id }
```

</details></ul>


Create something of class `f` inside of `i` at position `k`
- i : array
- k : atom  (string or number)
- ?f : function name  (defaults to `List`).
===### has??
- ?f : function name  (defaults to `List`).

<ul><details><summary><tt>has()</tt></summary>

```awk
function has(i,k,f) { 
   f=f?f:"List";i[k][0]; @f(i[k]); delete i[k][0]; return k}
```

</details></ul>


Like `has`, but accepts one constructor argument `x`.
- i : array
- k : atom  (string or number)
- f : function name  (defaults to `List`).
- x : any (something to be passed as `f(i,x)`)
===### haS??
- x : any (something to be passed as `f(i,x)`)

<ul><details><summary><tt>haS()</tt></summary>

```awk
function haS(i,k,f,x)  { i[k][0]; @f(i[k],x); delete i[k][0] }
```

</details></ul>


Like `has`, but accepts two constructor arguments `x` and `y`..
- i : array
- k : atom  (string or number)
- f : function name  (defaults to `List`).
- x : any (something to be passes as `f(i,x,y)`)
- y : any (something to be passed as `f(i,x,y)`)
===### hAS??
- y : any (something to be passed as `f(i,x,y)`)

<ul><details><summary><tt>hAS()</tt></summary>

```awk
function hAS(i,k,f,x,y) { i[k][0]; @f(i[k],x,y); delete i[k][0] }
```

</details></ul>


Add a new instances of class `f` at the end of `i`.
- i : array
- f : constructor function
===### new??
- f : constructor function

<ul><details><summary><tt>new()</tt></summary>

```awk
function new(i,f,    k) { k=length(i)+1; has(i,k,f); return k }
```

</details></ul>


## Testing
===### rogues??
Print local variables, escaped from functions

<ul><details><summary><tt>rogues()</tt></summary>

```awk
function rogues(   s) { f
  for(s in SYMTAB) if (s ~ /^[A-Z]/)  print "#W> Global: " s>"/dev/stderr" 
  for(s in SYMTAB) if (s ~ /^[_a-z]/) print "#W> Rogue: " s>"/dev/stderr" }
```

</details></ul>


Run the functions names in the comma-separated string `s`.
-  s : string; command separated function names.
===### tests??
-  s : string; command separated function names.

<ul><details><summary><tt>tests()</tt></summary>

```awk
function tests(s,     a,i,f) {
  split(s,a,",") 
  for(i in a) {
     f=a[i]
     if(f in FUNCTAB)
       @f(s) }}
```

</details></ul>


Print "PASS" if `got` same as `want1` (and print "FAIL" otherwise).
- what : string; some id tag
- got : value1
- want : value2
===### ok??
- want : value2

<ul><details><summary><tt>ok()</tt></summary>

```awk
function ok(f,got,want,   epsilon,     good,s) {
  epsilon = epsilon ? epsilon : AU.test.epsilon
  if (typeof(want) == "number") 
    good = abs(want - got)/(want + 10^-32)  < epsilon
  else
    good = want == got;
  s= "#TEST:\t"(good?"PASSED":"FAILED") "\t" i "\t" want "\t" got 
  print(good ? green(s) : red(s)) }
```

</details></ul>


## Maths
- x : number
===### abs??
- x : number

<ul><details><summary><tt>abs()</tt></summary>

```awk
function abs(x) { return x>=0 ? x : -1*x}
```

</details></ul>


Sample from a Gaussian.
- ?mu : number; defaults to 0.
- ?sd : number; defaults to 1.
===### z??
- ?sd : number; defaults to 1.

<ul><details><summary><tt>z()</tt></summary>

```awk
function z(mu,sd) {
  mu = mu?mu:0
  sd = sd?sd:1  
  return mu + sd*sqrt(-2*log(rand()))*cos(2*Gold.pi*rand())}
```

</details></ul>


## Printing
- a : array
- ?p : optional prefix
===### o??
- ?p : optional prefix

<ul><details><summary><tt>o()</tt></summary>

```awk
function o(a,p,     i,sep,s) {
  for(i in a) {s = s sep a[i]; sep=","}
  print p s }
```

</details></ul>


Print arrays, recursively. 
Show keys in sorted order.
- x : anything; what to print.
- p : string; something to print before each item
===### oo??
- p : string; something to print before each item

<ul><details><summary><tt>oo()</tt></summary>

```awk
function oo(x,p,   pre,      j,txt) {
  txt = pre ? pre : (p Gold.dot)
  ooSortOrder(x)
  for(j in x)  
    if (j !~ /^_/) {
      if (isarray(x[j]))   {
        print(txt j"" )
        oo(x[j],"","|  " pre)
      } else
        print(txt j (x[j]==""?"": ": " x[j])) } }
```

</details></ul>


## Reading
Returns zero at end of files.
Complain if file is missing. Kill comments `#` and spaces.
Split lines on commas. If records split over more than one 
line (and end in a comma) concat that line to the next.
Coerce numeric strings to numbers.
- a : array; containing the next record. 
- ?f : filename; defaults to standard input.
===### csv??
- ?f : filename; defaults to standard input.

<ul><details><summary><tt>csv()</tt></summary>

```awk
function csv(a,f,     b4, g,txt,i,old,new) {
  f = f ? f : "-"             
  g = getline < f
  if (g< 0) { print "#E> Missing f ["f"]"; exit 1 } # file missing
  if (g==0) { close(f) ; return 0 }       # end of file                   
  txt = b4 $0                             # combine with prior
  gsub(/[ \t]+/,"",txt)
  if (txt ~ /,$/) { return csv(a,f,txt) } # continue txt into next
  sub(/#.*/, "", txt)                    # kill whitespace,comments    
  if (!txt)       { return csv(a,f,txt) } # skip blanks
  split(txt, a, ",")                      # split on "," into "a"
  for(i in a) {
     old = a[i]
     new = a[i]+0
     a[i] = (old == new) ? new : old
  }
  return 1 } 
```


