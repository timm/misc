---
title: bins.fun
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

# bins.fun



```awk
   1.  @include "lib"
   2.  @include "the"
   3.  @include "table"
   4.  @include "some"
```

sadasd
adasd

this is unsuper. it should be followed by super

needs a pre-processor cols (to prune sillies)

cat | clean | cols | ranges | super | rows | cluster | privitize | contrast

repeat the above without super 

```awk
   5.  function Bins0(i) {
   6.    i.data = "data/weather" DOT "csv"
   7.    i.sep  = ","
   8.    i.step = 0.5
   9.    i.max = 256
  10.    i.magic = 2.56
  11.    i.cohen = 0.3
  12.    i.trivial = 0.3
  13.  }
```

-----------------------
# some extensions to the basic table stuff
```awk
  14.  function TableChop(i,   c) {
  15.    for(c in i.nums)  
  16.      TableChop1(i,c,i.nums[c]) 
  17.  }
  18.  function TableChop1(i,c,some,    r,cutter,cut,x,rs) {
  19.    Cuts0(cutter, some)
  20.    Cuts(cutter,some)
  21.    rs  = l(i.rows)
  22.    cut = 1
  23.    cellsort(i.rows, c)
  24.    for(r=1; r<=rs; r++)  {
  25.      x = i.rows[r].cells[c]
  26.      if (x != "?") {
  27.        if (x > some.cuts[cut]) 
  28.          if (cut< l(some.cuts) -1 )
  29.            cut++
  30.        i.rows[r].cells[c] = some.cuts[cut]  }}
  31.  }
```
---------------------
```awk
  32.  function Cuts0(i,some,    n) {
  33.    Object(i)
  34.    n         = l(some.has)
  35.    i.cohen   = G.cohen
  36.    i.start   = at(some,1)
  37.    i.stop    = at(some,n)
  38.    i.step    = int(n^G.step)
  39.    i.trivial = G.trivial 
  40.    i.epsilon = sd(some,1, n )*i.cohen
  41.  }
```

```awk
  42.  function Cuts(i,some,lo,hi,       
  43.                 j,cut,min,now,after,new) {
  44.    lo = lo ? lo : 1
  45.    hi = hi ? hi : l(some.has)
  46.    if (hi - lo > i.step) {
  47.      min  = sd(some,lo,hi)
  48.      for(j = lo + i.step; j<=hi-i.step; j++) {
  49.        now =  at(some,j)
  50.        after = at(some,j+1)
  51.        if (now != after && 
  52.            after - i.start > i.epsilon && 
  53.            i.stop - now    > i.epsilon &&
  54.            mid(some,j+1,hi) - mid(some,lo,j) > i.epsilon && 
  55.            min > (new = xpect(some,lo,j,hi)) * i.trivial) {
  56.              min = new
  57.              cut = j }}}
  58.    if (cut) {
  59.      Cuts(i,some,lo,    cut)
  60.      Cuts(i,some,cut+1, hi)
  61.    } else 
  62.      some.cuts[l(some.cuts)+1] = some.has[hi] 
  63.  }
```
---------------------
```awk
  64.  function binsMain( t) { 
  65.     Bins(G); argv(G); FS=G.sep 
  66.     Table(t)
  67.     TableRead(t,G.data)
  68.     TableChop(t)
  69.     TableDump(t)
  70.     rogues()
  71.  }
  72.  BEGIN { binsMain() }
```
