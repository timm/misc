#!/usr/bin/env ./fun
# vim: filetype=awk ts=2 sw=2 sts=2  et :

@include "lib.fun"
@include "table.fun"
@include "some.fun"

this is unsuper. it should be followed by super

needs a pre-processor cols (to prune sillies)

cat | clean | cols | ranges | super | rows | cluster | privitize | contrast

repeat the above without super 

function Bins0(i) {
  i.data = "data/weather" DOT "csv"
  i.sep  = ","
  i.step = 0.5
  i.max = 256
  i.magic = 2.56
  i.cohen = 0.3
  i.trivial = 0.3
}

-----------------------
# some extensions to the basic table stuff
function TableChop(i,   c) {
  for(c in i.nums)  
    TableChop1(i,c,i.nums[c]) 
}
function TableChop1(i,c,some,    r,cutter,cut,x,rs) {
  Cuts0(cutter, some)
  Cuts(cutter,some)
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
---------------------
function Cuts0(i,some,    n) {
  Object(i)
  n         = l(some.has)
  i.cohen   = G.cohen
  i.start   = at(some,1)
  i.stop    = at(some,n)
  i.step    = int(n^G.step)
  i.trivial = G.trivial 
  i.epsilon = sd(some,1, n )*i.cohen
}

function Cuts(i,some,lo,hi,       
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
    Cuts(i,some,lo,    cut)
    Cuts(i,some,cut+1, hi)
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
function cellsort(lst,k) { 
  SORT=k; return asort(lst,lst,"cellcompare") 
}
function cellcompare(i1,v1,i2,v2,  l,r) {
  l = v1["cells"][SORT]
  r = v2["cells"][SORT]
  if (l < r) return -1
  if (l == r) return 0
  return 1 
}

BEGIN { binsMain() }
