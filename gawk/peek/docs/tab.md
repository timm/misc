#  tab.gold
- [vim: ft=awk ts=2 sw=2 et :](#vimftawktsswet) : @include "lib"
    - [Row #################################################](#row)
    - [## Constructor](#constructor)
  - [Constructor](#constructor) : function Row(i:untyped) {
    - [## Distance between two rows](#distancebetweentworows)
  - [Distance between two rows](#distancebetweentworows) : function _Dist(i:Row,j:Row, tab, cols,  c,pos,x,y,d,d1,n) {
    - [Table ########################################](#table)
    - [## Constructor](#constructor)
  - [Constructor](#constructor) : function Tab(i:untyped) {
    - [## Load a csv file `f` into the table `i`](#loadacsvfilefintothetablei)
  - [Load a csv file `f` into the table `i`](#loadacsvfilefintothetablei) : function _Load(i:Tab, f:fname) {
    - [## Update `i` with `a`. First update creates the column headers.](#updateiwithafirstupdatecreatesthecolumnheaders)
  - [Update `i` with `a`. First update creates the column headers.](#updateiwithafirstupdatecreatesthecolumnheaders) : function _Add(i:Tab, a:array) {
      - [### _Header](#header)
    - [_Header](#header) : ## Initialize columns in a table.
  - [Column names containing `?` become `Info` columns.](#columnnamescontainingbecomeinfocolumns) : ## Column names containing `<>:` are `Num`bers (and all others are `Sym`s).
  - [Dependent variables (stored in `ys`) are marked with `<>!`](#dependentvariablesstoredinysaremarkedwith) : ## and all other are independent variables (stored in `xs`).
  - [Klass names are marked in `!`.](#klassnamesaremarkedin) : ## - i : Tab
  - [- a : array of column names.](#aarrayofcolumnnames) : function _Header(i,a,   where, what, j) {
    - [## Add an row at some random index within `rows`.](#addanrowatsomerandomindexwithinrows)
  - [Add an row at some random index within `rows`.](#addanrowatsomerandomindexwithinrows) : function _Data(i:Tab, a:array,    r,j) {
    - [_Dist](#dist)
    - [_Far](#far)
    - [_Around](#around)
    - [## Copy the structure of `i` into a new table `j`.](#copythestructureofiintoanewtablej)
  - [Copy the structure of `i` into a new table `j`.](#copythestructureofiintoanewtablej) : function _Clone(i:Tab, j:Tab) {
    - [Dendogram ########################################](#dendogram)
  - [# notes should be created in the tree;](#notesshouldbecreatedinthetree)
- [notes should be created in the tree;](#notesshouldbecreatedinthetree) : ## Constructor for a tree of clusters
  - [Constructor for a tree of clusters](#constructorforatreeofclusters) : function TreeNode(i) {
  - [_Descend](#descend)
  - [_Add](#add)


# vim: ft=awk ts=2 sw=2 et :
@include "lib"
@include "col"

### Row #################################################

### ## Constructor

<ul><details><summary><tt>## Constructor()</tt></summary>

```awk
## Constructor
function Row(i:untyped) {
  Object(i)
  i.p=2
  has(i,"cells")
  has(i,"ranges") }
```

</details></ul>

### ## Distance between two rows

<ul><details><summary><tt>## Distance between two rows()</tt></summary>

```awk
## Distance between two rows
function _Dist(i:Row,j:Row, tab, cols,  c,pos,x,y,d,d1,n) {
  for(c in cols) {
    pos = tab.cols[c].pos
    x   = i.cells[pos]
    y   = j.cells[pos]
    d1  = (x=="?" && y=="?") ? 1 : dist(tab.cols[c], x,y)
    d  += d1^i.p
    n++ }
  return (d/n)^(1/i.p) }
```

</details></ul>

### Table ########################################

### ## Constructor

<ul><details><summary><tt>## Constructor()</tt></summary>

```awk
## Constructor
function Tab(i:untyped) {
  Object(i); i.is = "Tab"
  i.klass   = ""
  i.use     = "xs"
  i.far     = 0.9
  has(i,"tree","TreeNode")
  has(i,"rows"); has(i,"cols"); has(i,"names")
  has(i,"info"); has(i,"xs");   has(i,"ys") }
```

</details></ul>

### ## Load a csv file `f` into the table `i`

<ul><details><summary><tt>## Load a csv file `f` into the table `i`()</tt></summary>

```awk
## Load a csv file `f` into the table `i`
function _Load(i:Tab, f:fname) {
  while(csv(record,f)) {  add(i,record)} }
```

</details></ul>

### ## Update `i` with `a`. First update creates the column headers.

<ul><details><summary><tt>## Update `i` with `a`. First update creates the column headers.()</tt></summary>

```awk
## Update `i` with `a`. First update creates the column headers.
function _Add(i:Tab, a:array) {
  if ("cells" in a) return TabAdd(i, a.cells)
  length(i.cols) ?  TabData(i,a) : TabHeader(i,a) }
```

</details></ul>

#### ### _Header

<ul><details><summary><tt>### _Header()</tt></summary>

```awk
### _Header
## Initialize columns in a table.
## Column names containing `?` become `Info` columns.
## Column names containing `<>:` are `Num`bers (and all others are `Sym`s).
## Dependent variables (stored in `ys`) are marked with `<>!` 
## and all other are independent variables (stored in `xs`).
## Klass names are marked in `!`.
## - i : Tab
## - a : array of column names.
function _Header(i,a,   where, what, j) {
  for(j=1; j<=length(a); j++) {
    i.names[j] = a[j]
    if (a[j] ~ /\?/) {
      what="Info"
      where="info"
    } else {
      what = a[j] ~ /[:<>]/ ?  "Num" : "Sym"
      where= a[j] ~ /[!<>]/ ?  "ys"  : "xs"
    }
    hAS(i.cols, j, what, a[j],j)   
    i[where][j]
    if (a[j]~/!/) i.klass = j }}
```

</details></ul>

### ## Add an row at some random index within `rows`.

<ul><details><summary><tt>## Add an row at some random index within `rows`.()</tt></summary>

```awk
## Add an row at some random index within `rows`.
function _Data(i:Tab, a:array,    r,j) {
  r = 1E6*rand()
  has(i.rows, r, "Row")
  for(j=1; j<=length(a); j++) 
    i.rows[r].cells[j] = add(i.cols[j], a[j]) 
  TreeNodeAdd(i.tree, i, r) }
```

</details></ul>

### _Dist

<ul><details><summary><tt>_Dist()</tt></summary>

```awk
function _Dist(i,r1,r2) {
  return  dist(i.rows[r1], i.rows[r2], i[i.use]) }
```

</details></ul>

### _Far

<ul><details><summary><tt>_Far()</tt></summary>

```awk
function _Far(i,r1,     n,all1 {
  n= _Around(i,r1,all) out) 
  return out[int(n*i.far)].row }
```

</details></ul>

### _Around

<ul><details><summary><tt>_Around()</tt></summary>

```awk
function _Around(i,r1,out,   r2) {
  for(r2 in i.rows) 
    if(r1 != r2) {
       out.row = r2
       out.d   = _Dist(r1, r2) }
  return keysort(out,"d") }
```

</details></ul>

### ## Copy the structure of `i` into a new table `j`.

<ul><details><summary><tt>## Copy the structure of `i` into a new table `j`.()</tt></summary>

```awk
## Copy the structure of `i` into a new table `j`.
function _Clone(i:Tab, j:Tab) {
  Tab(j)
  TabHeader(j, i.names) }
```

</details></ul>

### Dendogram ########################################

## # notes should be created in the tree;

<ul><details><summary><tt># notes should be created in the tree;()</tt></summary>

```awk
# notes should be created in the tree;
## Constructor for a tree of clusters
function TreeNode(i) {
  Object(i)
  i.enough=64
  i.is = "TreeNode"
  i.c=i.lo=i.hi=i.mid = ""
  has(i,"all")
  has(i,"upper")
  has(i,"lower")
}
function _X(i,t, r     a,b,x) {
   a= TabDist(t,r,i.lo)
   b= TabDist(t,r,i.hi)
   x= (a^2 + i.c^2 - b^2)/(2*i.c)
   return max(0, min(1, x)) }
```

</details></ul>

## _Descend

<ul><details><summary><tt>_Descend()</tt></summary>

```awk
function _Descend(i,t,d,r) {
  return _Add(i[ d < i.mid ? "lower" : "upper" ], 
              t, r) }
```

</details></ul>

## _Add

<ul><details><summary><tt>_Add()</tt></summary>

```awk
function _Add(i, t, r.   n,one,x,tmp) {
  push(i.all,  r)
  if (length(i.all) == i.enough)  {
    i.lo = TabFar(t, r)
    i.hi = TabFar(t, i.lo )
    i.c  = TabDist(t, i.lo, i.hi)
    for(one in i.all) {
      tmp[one]  = x = _X(i,t,one)
      i.mid    += x/2 
    }
    has(i,"upper","TreeNode")
    has(i,"lower","TreeNode")
    for (one in tmp) 
      _Descend(i,t, tmp[one], one) 
  }
  if (length(i.all)>i.enough) 
    return _Descend(i,t, _X(i,t,r),r)
  return i.id
}
 function _Print(i,         lvl,pre) {
   print pre length(i.all)
   if (length(i.lower)) _Print(i.lower, lvl+1, "|.. " pre)
   if (length(i.upper)) _Print(i.upper, lvl+1, "|.. " pre)
}
```

</details></ul>
