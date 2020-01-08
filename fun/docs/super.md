---
title: super.fun
---


<button class="button button1"> 
	<a href="/fun/index">home</a> 
</button><button class="button button2"> 
	<a href="/fun/INSTALL">install</a>
</button><button class="button button1"> 
	<a href="/fun/ABOUT">doc</a> 
</button><button class="button button2"> 
	<a href="http://github.com/timm/fun/issues">discuss</a> 
</button><button class="button button1">b1 
	<a href="/fun/LICENSE">license</a> 
</button><br>

# super.fun



```awk
   1.  @include "lib"
   2.  @include "some"
```

```awk
   3.  function Super(i) {
   4.    i.data = "data/weather" DOT "csv"
   5.    i.sep  = ","
   6.    i.step = 0.5
   7.    i.max = 256
   8.    i.magic = 2.56
   9.    i.cohen = 0.3
  10.    i.trivial = 0.3
  11.  }
```
-------------------------
```awk
  12.  function Table(i) {
  13.    Object(i)
  14.    has(i,"rows")
  15.    has(i,"names")
  16.    has(i,"nums") 
  17.  }
  18.  function TableRead(i,f) { lines(i,f, "Table1") }
```

```awk
  19.  function Table1(i,r,lst,      c,x) {
  20.    if (r>1)  
  21.      return hasss(i.rows,r-1,"Row",lst,i)
  22.    for(c in lst)  {
  23.      x = i.names[c] = lst[c]
  24.      if (x ~ /[\$<>]/) 
  25.        hass(i.nums,c,"Some",c) }
  26.  }
  27.  function TableDump(i,   r) {
  28.    print(cat(i.names))
  29.    for(r in i.rows)
  30.      print(cat(i.rows[r].cells)) 
  31.  }
  32.  function TableChop(i,   c) {
  33.    for(c in i.nums)  
  34.      TableChop1(i,c,i.nums[c]) 
  35.  }
  36.  function TableChop1(i,c,some,    r,cutter,cut,x,rs) {
  37.    Cuts(cutter, some)
  38.    CutsUp(cutter,some)
  39.    rs  = l(i.rows)
  40.    cut = 1
  41.    cellsort(i.rows, c)
  42.    for(r=1; r<=rs; r++)  {
  43.      x = i.rows[r].cells[c]
  44.      if (x != "?") {
  45.        if (x > some.cuts[cut]) 
  46.          if (cut< l(some.cuts) -1 )
  47.            cut++
  48.        i.rows[r].cells[c] = some.cuts[cut]  }}
  49.  }
```
_______________________________
```awk
  50.  function Row(i,lst,t,     x,c) {
  51.    Object(i)
  52.    has(i,"cells")
  53.    for(c in t.names) {
  54.      x = lst[c]
  55.      if (x != "?") {
  56.        if (c in t.nums) {
  57.           x += 0
  58.           Some1(t.nums[c], x) }
  59.        i.cells[c] = x }}
  60.  }
```
---------------------
```awk
  61.  function Cuts(i,some,    n) {
  62.    Object(i)
  63.    n         = l(some.has)
  64.    i.cohen   = G.cohen
  65.    i.start   = at(some,1)
  66.    i.stop    = at(some,n)
  67.    i.step    = int(n^G.step)
  68.    i.trivial = G.trivial 
  69.    i.epsilon = sd(some,1, n )*i.cohen
  70.  }
```

```awk
  71.  function CutsUp(i,some,lo,hi,       
  72.                 j,cut,min,now,after,new) {
  73.    lo = lo ? lo : 1
  74.    hi = hi ? hi : l(some.has)
  75.    if (hi - lo > i.step) {
  76.      min  = sd(some,lo,hi)
  77.      for(j = lo + i.step; j<=hi-i.step; j++) {
  78.        now =  at(some,j)
  79.        after = at(some,j+1)
  80.        if (now != after && 
  81.            after - i.start > i.epsilon && 
  82.            i.stop - now    > i.epsilon &&
  83.            mid(some,j+1,hi) - mid(some,lo,j) > i.epsilon && 
  84.            min > (new = xpect(some,lo,j,hi)) * i.trivial) {
  85.              min = new
  86.              cut = j }}}
  87.    if (cut) {
  88.      CutsUp(i,some,lo,    cut)
  89.      CutsUp(i,some,cut+1, hi)
  90.    } else 
  91.      some.cuts[l(some.cuts)+1] = some.has[hi] 
  92.  }
```
---------------------
```awk
  93.  function binsMain( t) { 
  94.     Bins(G); argv(G); FS=G.sep 
  95.     Table(t)
  96.     TableRead(t,G.data)
  97.     TableChop(t)
  98.     TableDump(t)
  99.     rogues()
 100.  }
 101.  BEGIN { binsMain() }
```
