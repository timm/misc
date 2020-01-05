#!/usr/bin/env ./fun
# vim: filetype=awk ts=2 sw=2 sts=2  et :

@include "lib"
@include "some"

function Super(i) {
  i.data = "data/weather" DOT "csv"
  i.sep  = ","
  i.step = 0.5
  i.max = 256
  i.magic = 2.56
  i.cohen = 0.3
  i.trivial = 0.3
}
-------------------------
function Table(i) {
  Object(i)
  has(i,"rows")
  has(i,"names")
  has(i,"nums") 
}
function TableRead(i,f) { lines(i,f, "Table1") }

function Table1(i,r,lst,      c,x) {
  if (r>1)  
    return hasss(i.rows,r-1,"Row",lst,i)
  for(c in lst)  {
    x = i.names[c] = lst[c]
    if (x ~ /[\$<>]/) 
      hass(i.nums,c,"Some",c) }
}
function TableDump(i,   r) {
  print(cat(i.names))
  for(r in i.rows)
    print(cat(i.rows[r].cells)) 
}
function TableChop(i,   c) {
  for(c in i.nums)  
    TableChop1(i,c,i.nums[c]) 
}
function TableChop1(i,c,some,    r,cutter,cut,x,rs) {
  Cuts(cutter, some)
  CutsUp(cutter,some)
  rs  = l(i.rows)
  cut = 1
  cellsort(i.rows, c)
  for(r=1; r<=rs; r++)  {
    x = i.rows[r].cells[c]
    if (x != "?") {
      if (x > some.cuts[cut]) 
        if (cut< l(some.cuts) -1 )
          cut++
      i.rows[r].cells[c] = some.cuts[cut]  }}
}
_______________________________
function Row(i,lst,t,     x,c) {
  Object(i)
  has(i,"cells")
  for(c in t.names) {
    x = lst[c]
    if (x != "?") {
      if (c in t.nums) {
         x += 0
         Some1(t.nums[c], x) }
      i.cells[c] = x }}
}
---------------------
function Cuts(i,some,    n) {
  Object(i)
  n         = l(some.has)
  i.cohen   = G.cohen
  i.start   = at(some,1)
  i.stop    = at(some,n)
  i.step    = int(n^G.step)
  i.trivial = G.trivial 
  i.epsilon = sd(some,1, n )*i.cohen
}

function CutsUp(i,some,lo,hi,       
               j,cut,min,now,after,new) {
  lo = lo ? lo : 1
  hi = hi ? hi : l(some.has)
  if (hi - lo > i.step) {
    min  = sd(some,lo,hi)
    for(j = lo + i.step; j<=hi-i.step; j++) {
      now =  at(some,j)
      after = at(some,j+1)
      if (now != after && 
          after - i.start > i.epsilon && 
          i.stop - now    > i.epsilon &&
          mid(some,j+1,hi) - mid(some,lo,j) > i.epsilon && 
          min > (new = xpect(some,lo,j,hi)) * i.trivial) {
            min = new
            cut = j }}}
  if (cut) {
    CutsUp(i,some,lo,    cut)
    CutsUp(i,some,cut+1, hi)
  } else 
    some.cuts[l(some.cuts)+1] = some.has[hi] 
}
---------------------
function binsMain( t) { 
   Bins(G); argv(G); FS=G.sep 
   Table(t)
   TableRead(t,G.data)
   TableChop(t)
   TableDump(t)
   rogues()
}
BEGIN { binsMain() }
