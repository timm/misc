#!/usr/bin/env ./gold
# vim: filetype=awk ts=2 sw=2 sts=2  et :

@include "my"
@include "lib"
@include "num"
@include "sym"
@include "shared"

function DivX(i,rows,r) { return rows[r].cells[i.x] }
function DivY(i,rows,r) { return rows[r].cells[i.y] }

function DivSort(i,k) {
  ROWSORT=k
  return asort(i.rows,i.rows,"__asortDiv")
}
function __asortRows(i1,row1,i2,row2,    l,r) {
  l = row1.cells[ ROWSORT ] + 0
  r = row2.cells[ ROWSORT ] + 0
  if (l <  r) return -1
  if (l == r) return 0
  return 1 
} 
function Div(i,a) {
  is(i,"Div")
  i.x        = 1
  i.cohen    = 0.2
  i.step     = 0.5
  i.maxDepth = 15
  i.step     = length(a)^i.step
  i.epsilon  = 0
  i.trivial  = 1.025
 }
function DivReady(i,a)
  DivSort(i,i.x)
  i.y = i.y ? i.y : length(a[1].cells)
  has(i,"xall","Num")
  y = DivY(i,a,1)
  has(i,"yall", typeof(y)=="number" ? "Num" : "Sym")
  for(r in a) {
    x = DivX(i,a,r)
    y = DivY(i,a,r)
    if (x ~ MY.skip) next
    i.start = i.start=="" ? i.start : x  
    i.stop  = x
    Add(i.xall, x)
    Add(i.yall, y) 
  }  
  i.epsilon = i.epsilon ? i.epsilon : Var(i.xall)*i.cohen
  has(i,"cuts")
}
function DivCut(i,a,lo,hi,lvl,xr,yr, xr1,xl1,yr1,yl1,
               min,yis,min,r,x,y,new,cut) {
  if (hi - lo <=  i.step)  return 0
  min = Var(yr)
  Num(xl)
  Num(yl)
  yis = i.yis; &yis(yl)
  for(r=lo; r<=hi-i.step; r++) {
    x  = DivX(i,a,r)
    x1 = DivX(i,a,r+1)
    y  = DivY(i,a,r)
    Add(xl,x); Dec(xr, x)
    Add(yl,y); Dec(yr, y)
    if (x ~ MY.skip  ) next
    if (x == x1 )      next # x1 "?"
    if (x - i.start <= i.epsilon)        next
    if (i.stop - x  <= i.epsilon)        next
    if  (Mid(xr) - Mid(xl) <= i.epsilon) next
    new = Xpect(yl,yr) 
    if (new*i.trivial < min)  {
       min = new
       cut = j
       copy(xr, xr1); copy(xl, xl1)
       copy(yr, yrl); copy(yl, yl1) }}
  return cut
}

            
        
 
           
}
