# Lib

## Maths

```awk
function abs(x)  { return x>=0? x: -1*x }
```
## Lists

Misc

```awk
function any(a)    { return int(0.5 + rand()*length(a)) }
function push(a,x) { a[length(a)+1] = x; return x }
```

Display nested lists.

```awk
function oo(a,prefix,    indent,   i,txt) {
  txt = indent ? indent : (prefix AU["dot"] )
  ooSortOrder(a)
  for(i in a)  {
    if (isarray(a[i]))   {
      print(txt i"" )
      oo(a[i],"","|  " indent)
    } else
      print(txt i (a[i]==""?"": ": " a[i])) }
}
function ooSortOrder(a, i) {
  for (i in a)
   return PROCINFO["sorted_in"] =\
     typeof(i+1)=="number" ? "@ind_num_asc" : "@ind_str_asc"
}
```

Sorting

```awk
function keysort(a,k) {
  APE.keysort = k
  return asort(a,a,"keysorter")
}
function keysorter(i1,x,i2,y) {
  return compare(x[ APE.keysort ] + 0,
                 y[ APE.keysort ] + 0)
} 
function compare(x,y) {
  if (x < y) return -1
  if (x == y) return 0
  return 1
}
```

## Debugging

```awk
function rogues(    s) {
  for(s in SYMTAB) 
    if (s ~ /^[A-Z][a-z]/) 
      print "#W> Global " s>"/dev/stderr"
  for(s in SYMTAB) 
    if (s ~ /^[_a-z]/    ) 
      print "#W> Rogue: " s>"/dev/stderr"
}
```
