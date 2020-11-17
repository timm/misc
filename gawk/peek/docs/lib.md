#  lib.gold
- [red](#red)
- [push](#push)
- [keysort](#keysort)
- [__keysort](#__keysort)
- [sortCompare](#sortCompare)
- [copy](#copy)
- [ooSortOrder](#ooSortOrder)

vim: ft=awk ts=2 sw=2 et :

Miscellaneous support code.

### Globals #################################################
# There is only one global.
BEGIN { List(Gold); 
  Gold.test.epsilon=0.00001
  Gold.pi= 3.141592653
  Gold.e = 2.718281828
  Gold.dot=sprintf("%c",46)  }

### Object stuff ############################################

## Initialize an empty list
function List(i:untyped) { 
  split("",i,"") }

## Initialize a new object, give it a unique id (in `i.id`)
function Object(i:untyped) { 
  List(i); i.id = ++Gold.id }

## Create something of class `f` inside of `i` at position `k`
function has(i:untyped, k:atom, f:?fname) { 
   f=f?f:"List";i[k][0]; @f(i[k]); delete i[k][0]; return k}

## Like `has`, but accepts one constructor argument `x`.
function haS(i:untyped, k:atom, f:?fname, x:any)  { 
  i[k][0]; @f(i[k],x); delete i[k][0] }

## Like `has`, but accepts two constructor arguments `x` and `y`..
function hAS(i:untyped, k:atom, f:fname, x:any, y:any) { 
  i[k][0]; @f(i[k],x,y); delete i[k][0] }

## Add a new instances of class `f` at the end of `i`.
function new(i:array, f:fname, k) { k=length(i)+1; has(i,k,f); return k }

## If `x` can be coerced number, return that. Else return `x`.
function asNum(x:atom,    y) {
  y = x+0
  return x==y ? x : y }

### Testing stuff ############################################

## Print local variables, escaped from functions
function rogues(   s) { f
  for(s in SYMTAB) if (s ~ /^[A-Z]/)  print "#W> Global: " s>"/dev/stderr" 
  for(s in SYMTAB) if (s ~ /^[_a-z]/) print "#W> Rogue: "  s>"/dev/stderr" }

## Run the functions names in the comma-separated string `s`.
function tests(s:string,     a,i,f) {
  split(s,a,",") 
  for(i in a) {
     f=a[i]
     if(f in FUNCTAB)
       @f(s) }}

## Print "PASS" if `got` same as `want1` (and print "FAIL" otherwise).
function ok(s:string, got:atom, want:atom,   epsilon:number,       good) {
  epsilon = epsilon ? epsilon : Gold.test.epsilon
  if (typeof(want) == "number") 
    good = abs(want - got)/(want + 10^-32)  < epsilon
  else
    good = want == got;
  s= "#TEST:\t"(good?"PASSED":"FAILED") "\t" i "\t" want "\t" got " : " s
  print(good ? green(s) : red(s)) }

 red

<ul><details><summary><tt> red()</tt></summary>

```awk
function red(x)   { return "\033[31m"x"\033[0m" }
function green(x) { return "\033[32m"x"\033[0m" }
```

</details></ul>

### Array stuff ############################################

 push

<ul><details><summary><tt> push()</tt></summary>

```awk
function push(a,i,x) { a[length(a)+1] = x; return x }
```

</details></ul>

 keysort

<ul><details><summary><tt> keysort()</tt></summary>

```awk
function keysort(a,k) { 
  Gold["keysort"]=k; return asort(a,a,"__keysort") }
```

</details></ul>

 __keysort

<ul><details><summary><tt> __keysort()</tt></summary>

```awk
function __keysort(i1,x,i2,y) {
  return sortCompare(x[ Gold["keysort"] ] + 0,
                     y[ Gold["keysort"] ] + 0) } 
```

</details></ul>

 sortCompare

<ul><details><summary><tt> sortCompare()</tt></summary>

```awk
function sortCompare(x,y) { return x<y ? -1 : (x==y?0:1) }
```

</details></ul>

 copy

<ul><details><summary><tt> copy()</tt></summary>

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

## Return absolute value of `x`.
function abs(x:number)           { return x>0 ? x : -1*x }
function max(x:number, y:number) { return x>y ? x : y }
function min(x:number, y:number) { return x>y ? y : x }

## Sample from a Gaussian.
function z(mu:nummber|0, sd:number|0) {
  mu = mu?mu:0
  sd = sd?sd:1  
  return mu + sd*sqrt(-2*log(rand()))*cos(2*Gold.pi*rand())}

### Printing stuff #########################################

## Simple printing of a flat array
function o(a:array, prefix:string|"",     i,sep,s) {
  for(i in a) {s = s sep prefix a[i]; sep=","}
  return  s }

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

 ooSortOrder

<ul><details><summary><tt> ooSortOrder()</tt></summary>

```awk
function ooSortOrder(x, j) {
  for (j in x)
    return PROCINFO["sorted_in"] = \
      typeof(j + 1)=="number" ? "@ind_num_asc" : "@ind_str_asc" }
```

</details></ul>

### File stuff #########################################

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
