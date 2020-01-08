---
title: some.fun
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

# some.fun



```awk
   1.  @include "lib"
   2.  @include "the"
```
______________________________

```awk
   3.  function Some(i,pos) {
   4.    Object(i)
   5.    i.magic || Some0(i)
   6.    has(i,"has")
   7.    has(i,"cuts")
   8.    i.pos    = pos ? pos : 1
   9.    i.sorted = 0
  10.    i.n      = 0 
  11.  }
  12.  function Some0(i) {
  13.    i.magic = G.some.magic # 2.56
  14.    i.max   = G.some.max   # 256
  15.    i.small = G.stats.cliffs.small # 0.147
  16.  }
  17.  function Some1(i,x) {
  18.    if (x == "?") return
  19.    i.n++
  20.    if (i.n < i.max) {
  21.      i.has[ l(i.has)+1 ] = x
  22.      i.sorted=0
  23.    } else {
  24.      if (i.n == i.max) 
  25.        i.sorted = asort(i.has)
  26.      if (rand() < i.max/i.n)
  27.        i.has[ binChop(i.has,x) ] = x }
  28.  }
  29.  function sorted(i)  { 
  30.    if (!i.sorted) 
  31.      i.sorted=asort(i.has) 
  32.    return length(i.has)
  33.  }
  34.  function at(i,z)      { sorted(i);  return i.has[int(z)] }
  35.  function per(i,j,k,p) { return at(i,j + p*(k-j))   }
  36.  function mid(i,j,k)   { return at(i,j + .5*(k-j) ) }
  37.  function sd(i,j,k)    {
  38.    return abs(per(i,j,k,.9) - per(i,j,k,.1))/i.magic 
  39.  }
  40.  function xpect(i,j,m,k,   n) {
  41.    n=k-j+1
  42.    return (m-j)/n*sd(i,j,m) + (k-m -1)/n*sd(i,m+1,k) 
  43.  }
```

```awk
  44.  function SomeDiff(i,j,   k,la,lb,n,x,lo,hi,gt,lt) {
  45.    # Returns 1 if i,j differ by more than a small effect
  46.    la = sorted(i)
  47.    lb = sorted(j)
  48.    for(k in i.has) {
  49.      x= i.has[k]
  50.      lo= hi= binChop(j.has, x)
  51.      while(lo > 1 && i.has[lo] >= x) lo--
  52.      while(hi < n && i.has[hi] <= x) hi++
  53.      lt += la - hi + 1
  54.      gt += lo
  55.    }
  56.    return i.small < abs(gt - lt) / (la*lb)  
  57.  }
  58.  BEGIN { Some(i); argv(i); oo(i)}
```
