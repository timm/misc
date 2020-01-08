---
title: table.fun
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

# table.fun



```awk
   1.  function Table0(i) {
   2.    i.data = "data/weather" DOT "csv"
   3.    i.sep  = ","
   4.  }
   5.  function Table(i) {
   6.    Object(i)
   7.    has(i,"rows")
   8.    has(i,"names")
   9.    has(i,"nums") 
  10.  }
  11.  function TableRead(i,f) { lines(i,f, "Table1") }
```

```awk
  12.  function Table1(i,r,lst,      c,x) {
  13.    if (r>1)  
  14.      return hasss(i.rows,r-1,"Row",lst,i)
  15.    for(c in lst)  {
  16.      x = i.names[c] = lst[c]
  17.      if (x ~ /[\$<>]/) 
  18.        hass(i.nums,c,"Some",c) }
  19.  }
  20.  function TableDump(i,   r) {
  21.    print(cat(i.names))
  22.    for(r in i.rows)
  23.      print(cat(i.rows[r].cells)) 
  24.  }
```
_______________________________
```awk
  25.  function Row(i,lst,t,     x,c) {
  26.    Object(i)
  27.    has(i,"cells")
  28.    for(c in t.names) {
  29.      x = lst[c]
  30.      if (x != "?") {
  31.        if (c in t.nums) {
  32.           x += 0
  33.           Some1(t.nums[c], x) }
  34.        i.cells[c] = x }}
  35.  }
```
