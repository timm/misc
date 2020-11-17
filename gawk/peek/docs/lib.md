#  lib.gold
- [vim: ft=awk ts=2 sw=2 et :](#vimftawktsswet)
- [Miscellaneous support code.](#miscellaneoussupportcode)
      - [### Globals #################################################](#globals)
    - [Globals #################################################](#globals) : # There is only one global.
    - [Object stuff ############################################](#objectstuff)
    - [## Initialize an empty list](#initializeanemptylist)
  - [Initialize an empty list](#initializeanemptylist) : function List(i:untyped) { 
    - [## Initialize a new object, give it a unique id](#initializeanewobjectgiveitauniqueid)
  - [Initialize a new object, give it a unique id (in `i.id`)](#initializeanewobjectgiveitauniqueidiniid) : function Object(i:untyped) { 
    - [## Create something of class `f` inside of `i` at position `k`](#createsomethingofclassfinsideofiatpositionk)
  - [Create something of class `f` inside of `i` at position `k`](#createsomethingofclassfinsideofiatpositionk) : function has(i:untyped, k:atom, f:?fname) { 
    - [## Like `has`, but accepts one constructor argument `x`.](#likehasbutacceptsoneconstructorargumentx)
  - [Like `has`, but accepts one constructor argument `x`.](#likehasbutacceptsoneconstructorargumentx) : function haS(i:untyped, k:atom, f:?fname, x:any)  { 
    - [## Like `has`, but accepts two constructor arguments `x` and `y`..](#likehasbutacceptstwoconstructorargumentsxandy)
  - [Like `has`, but accepts two constructor arguments `x` and `y`..](#likehasbutacceptstwoconstructorargumentsxandy) : function hAS(i:untyped, k:atom, f:fname, x:any, y:any) { 
    - [## Add a new instances of class `f` at the end of `i`.](#addanewinstancesofclassfattheendofi)
  - [Add a new instances of class `f` at the end of `i`.](#addanewinstancesofclassfattheendofi) : function new(i:array, f:fname, k) { k=length(i)+1; has(i,k,f); return k }
    - [## If `x` can be coerced number, return that. Else return `x`.](#ifxcanbecoercednumberreturnthatelsereturnx)
  - [If `x` can be coerced number, return that. Else return `x`.](#ifxcanbecoercednumberreturnthatelsereturnx) : function asNum(x:atom,    y) {
    - [Testing stuff ############################################](#testingstuff)
    - [## Print local variables, escaped from s](#printlocalvariablesescapedfroms)
  - [Print local variables, escaped from functions](#printlocalvariablesescapedfromfunctions) : function rogues(   s) { f
    - [## Run the s names in the comma-separated string `s`.](#runthesnamesinthecommaseparatedstrings)
  - [Run the functions names in the comma-separated string `s`.](#runthefunctionsnamesinthecommaseparatedstrings) : function tests(s:string,     a,i,f) {
    - [## Print "PASS" if `got` same as `want1`](#printpassifgotsameaswant)
  - [Print "PASS" if `got` same as `want1` (and print "FAIL" otherwise).](#printpassifgotsameaswantandprintfailotherwise) : function ok(s:string, got:atom, want:atom,   epsilon:number,       good) {
    - [red](#red)
    - [Array stuff ############################################](#arraystuff)
      - [push](#push)
      - [keysort](#keysort)
      - [__keysort](#keysort)
      - [sortCompare](#sortcompare)
      - [copy](#copy)
    - [Maths stuff ############################################](#mathsstuff)
    - [## Return absolute value of `x`.](#returnabsolutevalueofx)
  - [Return absolute value of `x`.](#returnabsolutevalueofx) : function abs(x:number)           { return x>0 ? x : -1*x }
    - [## Sample from a Gaussian.](#samplefromagaussian)
  - [Sample from a Gaussian.](#samplefromagaussian) : function z(mu:nummber|0, sd:number|0) {
    - [Printing stuff #########################################](#printingstuff)
    - [## Simple printing of a flat array](#simpleprintingofaflatarray)
  - [Simple printing of a flat array](#simpleprintingofaflatarray) : function o(a:array, prefix:string|"",     i,sep,s) {
    - [## Print arrays, recursively, shows in sorted array, prepend with `prefix`.](#printarraysrecursivelyshowsinsortedarrayprependwithprefix)
  - [Print arrays, recursively, shows in sorted array, prepend with `prefix`.](#printarraysrecursivelyshowsinsortedarrayprependwithprefix) : function oo(x:array, prefix:string,   indent,      j,txt) {
    - [ooSortOrder](#oosortorder)
    - [File stuff #########################################](#filestuff)
    - [## Loop over a csv file `f`, setting the array `a` to the next record.](#loopoveracsvfilefsettingthearrayatothenextrecord)
  - [Loop over a csv file `f`, setting the array `a` to the next record.](#loopoveracsvfilefsettingthearrayatothenextrecord) : ## Returns zero at end of files.
  - [Complain if file is missing. Kill comments `#` and spaces.](#complainiffileismissingkillcommentsandspaces) : ## Split lines on commas. If records split over more than one 
  - [line (and end in a comma) concat that line to the next.](#lineandendinacommaconcatthatlinetothenext) : ## Coerce numeric strings to numbers.


# vim: ft=awk ts=2 sw=2 et :

# Miscellaneous support code.

#### ### Globals #################################################

<ul><details><summary><tt>### Globals #################################################()</tt></summary>

```awk
### Globals #################################################
# There is only one global.
BEGIN { List(Gold); 
  Gold.test.epsilon=0.00001
  Gold.pi= 3.141592653
  Gold.e = 2.718281828
  Gold.dot=sprintf("%c",46)  }
```

</details></ul>

### Object stuff ############################################

### ## Initialize an empty list

<ul><details><summary><tt>## Initialize an empty list()</tt></summary>

```awk
## Initialize an empty list
function List(i:untyped) { 
  split("",i,"") }
```

</details></ul>

### ## Initialize a new object, give it a unique id 

<ul><details><summary><tt>## Initialize a new object, give it a unique id ()</tt></summary>

```awk
## Initialize a new object, give it a unique id (in `i.id`)
function Object(i:untyped) { 
  List(i); i.id = ++Gold.id }
```

</details></ul>

### ## Create something of class `f` inside of `i` at position `k`

<ul><details><summary><tt>## Create something of class `f` inside of `i` at position `k`()</tt></summary>

```awk
## Create something of class `f` inside of `i` at position `k`
function has(i:untyped, k:atom, f:?fname) { 
   f=f?f:"List";i[k][0]; @f(i[k]); delete i[k][0]; return k}
```

</details></ul>

### ## Like `has`, but accepts one constructor argument `x`.

<ul><details><summary><tt>## Like `has`, but accepts one constructor argument `x`.()</tt></summary>

```awk
## Like `has`, but accepts one constructor argument `x`.
function haS(i:untyped, k:atom, f:?fname, x:any)  { 
  i[k][0]; @f(i[k],x); delete i[k][0] }
```

</details></ul>

### ## Like `has`, but accepts two constructor arguments `x` and `y`..

<ul><details><summary><tt>## Like `has`, but accepts two constructor arguments `x` and `y`..()</tt></summary>

```awk
## Like `has`, but accepts two constructor arguments `x` and `y`..
function hAS(i:untyped, k:atom, f:fname, x:any, y:any) { 
  i[k][0]; @f(i[k],x,y); delete i[k][0] }
```

</details></ul>

### ## Add a new instances of class `f` at the end of `i`.

<ul><details><summary><tt>## Add a new instances of class `f` at the end of `i`.()</tt></summary>

```awk
## Add a new instances of class `f` at the end of `i`.
function new(i:array, f:fname, k) { k=length(i)+1; has(i,k,f); return k }
```

</details></ul>

### ## If `x` can be coerced number, return that. Else return `x`.

<ul><details><summary><tt>## If `x` can be coerced number, return that. Else return `x`.()</tt></summary>

```awk
## If `x` can be coerced number, return that. Else return `x`.
function asNum(x:atom,    y) {
  y = x+0
  return x==y ? x : y }
```

</details></ul>

### Testing stuff ############################################

### ## Print local variables, escaped from s

<ul><details><summary><tt>## Print local variables, escaped from s()</tt></summary>

```awk
## Print local variables, escaped from functions
function rogues(   s) { f
  for(s in SYMTAB) if (s ~ /^[A-Z]/)  print "#W> Global: " s>"/dev/stderr" 
  for(s in SYMTAB) if (s ~ /^[_a-z]/) print "#W> Rogue: "  s>"/dev/stderr" }
```

</details></ul>

### ## Run the s names in the comma-separated string `s`.

<ul><details><summary><tt>## Run the s names in the comma-separated string `s`.()</tt></summary>

```awk
## Run the functions names in the comma-separated string `s`.
function tests(s:string,     a,i,f) {
  split(s,a,",") 
  for(i in a) {
     f=a[i]
     if(f in FUNCTAB)
       @f(s) }}
```

</details></ul>

### ## Print "PASS" if `got` same as `want1` 

<ul><details><summary><tt>## Print "PASS" if `got` same as `want1` ()</tt></summary>

```awk
## Print "PASS" if `got` same as `want1` (and print "FAIL" otherwise).
function ok(s:string, got:atom, want:atom,   epsilon:number,       good) {
  epsilon = epsilon ? epsilon : Gold.test.epsilon
  if (typeof(want) == "number") 
    good = abs(want - got)/(want + 10^-32)  < epsilon
  else
    good = want == got;
  s= "#TEST:\t"(good?"PASSED":"FAILED") "\t" i "\t" want "\t" got " : " s
  print(good ? green(s) : red(s)) }
```

</details></ul>

### red

<ul><details><summary><tt>red()</tt></summary>

```awk
function red(x)   { return "\033[31m"x"\033[0m" }
function green(x) { return "\033[32m"x"\033[0m" }
```

</details></ul>

### Array stuff ############################################

#### push

<ul><details><summary><tt>push()</tt></summary>

```awk
function push(a,i,x) { a[length(a)+1] = x; return x }
```

</details></ul>

#### keysort

<ul><details><summary><tt>keysort()</tt></summary>

```awk
function keysort(a,k) { 
  Gold["keysort"]=k; return asort(a,a,"__keysort") }
```

</details></ul>

#### __keysort

<ul><details><summary><tt>__keysort()</tt></summary>

```awk
function __keysort(i1,x,i2,y) {
  return sortCompare(x[ Gold["keysort"] ] + 0,
                     y[ Gold["keysort"] ] + 0) } 
```

</details></ul>

#### sortCompare

<ul><details><summary><tt>sortCompare()</tt></summary>

```awk
function sortCompare(x,y) { return x<y ? -1 : (x==y?0:1) }
```

</details></ul>

#### copy

<ul><details><summary><tt>copy()</tt></summary>

```awk
function copy(a,b,   j) { 
  for(j in a) 
    if(isarray(a[j]))  {
      has(b,j)
      copy(a[j], b[j]) 
    } else
      b[j] = a[j] 
}
```

</details></ul>

### Maths stuff ############################################

### ## Return absolute value of `x`.

<ul><details><summary><tt>## Return absolute value of `x`.()</tt></summary>

```awk
## Return absolute value of `x`.
function abs(x:number)           { return x>0 ? x : -1*x }
function max(x:number, y:number) { return x>y ? x : y }
function min(x:number, y:number) { return x>y ? y : x }
```

</details></ul>

### ## Sample from a Gaussian.

<ul><details><summary><tt>## Sample from a Gaussian.()</tt></summary>

```awk
## Sample from a Gaussian.
function z(mu:nummber|0, sd:number|0) {
  mu = mu?mu:0
  sd = sd?sd:1  
  return mu + sd*sqrt(-2*log(rand()))*cos(2*Gold.pi*rand())}
```

</details></ul>

### Printing stuff #########################################

### ## Simple printing of a flat array

<ul><details><summary><tt>## Simple printing of a flat array()</tt></summary>

```awk
## Simple printing of a flat array
function o(a:array, prefix:string|"",     i,sep,s) {
  for(i in a) {s = s sep prefix a[i]; sep=","}
  return  s }
```

</details></ul>

### ## Print arrays, recursively, shows in sorted array, prepend with `prefix`.

<ul><details><summary><tt>## Print arrays, recursively, shows in sorted array, prepend with `prefix`.()</tt></summary>

```awk
## Print arrays, recursively, shows in sorted array, prepend with `prefix`.
function oo(x:array, prefix:string,   indent,      j,txt) {
  txt = prefix ? prefix : (prefix Gold.dot)
  ooSortOrder(x)
  for(j in x)  
    if (j !~ /^_/) {
      if (isarray(x[j]))   {
        print(txt j"" )
        oo(x[j],"","|  " prefix)
      } else
        print(txt j (x[j]==""?"": ": " x[j])) } }
```

</details></ul>

### ooSortOrder

<ul><details><summary><tt>ooSortOrder()</tt></summary>

```awk
function ooSortOrder(x, j) {
  for (j in x)
    return PROCINFO["sorted_in"] = \
      typeof(j + 1)=="number" ? "@ind_num_asc" : "@ind_str_asc" }
```

</details></ul>

### File stuff #########################################

### ## Loop over a csv file `f`, setting the array `a` to the next record.

<ul><details><summary><tt>## Loop over a csv file `f`, setting the array `a` to the next record.()</tt></summary>

```awk
## Loop over a csv file `f`, setting the array `a` to the next record.
## Returns zero at end of files.
## Complain if file is missing. Kill comments `#` and spaces.
## Split lines on commas. If records split over more than one 
## line (and end in a comma) concat that line to the next.
## Coerce numeric strings to numbers.
function csv(a:array, f:string|"",       b4, g,txt,i,old,new) {
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

</details></ul>
