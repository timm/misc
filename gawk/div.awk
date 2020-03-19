#!/usr/bin/env ./gold
# vim: filetype=awk ts=2 sw=2 sts=2  et :

@include "my"
@include "lib"
@include "num"
@include "sym"
@include "shared"

function DivSort(i,rows,x) {
  ROWSORT=x
  return asort(rows,rows,"__asortDiv")
}
function __asortDiv(i1,row1,i2,row2,    l,r) {
  l = row1.cells[ ROWSORT ] + 0
  r = row2.cells[ ROWSORT ] + 0
  if (l <  r) return -1
  if (l == r) return 0
  return 1 
} 
function Div(i,x,y) {
  is(i,"Div")
  i.x        = x
  i.y        = y
  i.cohen    = 0.2
  i.step     = 0.5
  i.maxDepth = 15
  i.epsilon  = 0
  i.trivial  = 1.025
 }
function DivReady(i,rows,   r,x,y) {
  DivSort(i,rows, i.x)
  has(i, "all")
  has(i, "yall")
  has(i, "xall","Num")
  for(r in rows) {
    x = rows[r].cells[i.x]
    y = rows[r].cells[i.y]
    if (x ~ MY.skip) next
    if (y ~ MY.skip) next
    push(i.all,r)
    if (! length(i.yall))
      has(i, "yall", typeof(y)=="number" ? "Num" : "Sym")
    Add(i.xall, x)
    Add(i.yall, y) 
  }  
  i.start   = i.xall.lo
  i.stop    = i.xall.hi
  i.step    = i.step    ? i.step    : length(i.all)^i.step
  i.epsilon = i.epsilon ? i.epsilon : Var(i.xall)*i.cohen
}
function DivY(    i,rows,r)   { return rows[i.all[r]].cells[i.y] }
function DivX(    i,rows,r)   { return rows[i.all[r]].cells[i.x] }
function DivRange(i,rows,r,z) {        rows[i.all[r]].range[i.x]=z }

function DivMain(i,rows) {
  DivReady(i,rows)
  return DivCut1(i,rows,1,length(i.all),i.xall,i.yall)
}
function DivCut1(i,rows,lo,hi,xall,yall,
                 cut,xl,yl,xr,yr,lox,j) {
  cut = DivArgmin(i,rows,lo,hi,xall,yall,xl,yl,xr,yr)
  if (cut) {
    DivCut1(i,rows, lo,  cut,xl,yl)
    DivCut1(i,rows, cut+1,hi,xr,yr)
  } else  {
    lox = DivX(i, rows, lo)
    for(j=lo;j<=hi;j++)
      DivRange(i,rows,j, lox) }
}
function DivArgmin(i,rows,lo,hi,xr,yr, xl1,yl1,xr1,yr1,
               min,xl,yl,r,s,x,x1,y,new,cut,after) {
  if (hi - lo <= i.step) return 0
  min = Var(yr)
  Num(xl)
  Num(yl)
  for(r=lo; r<=hi; r++) {
    x = DivX(i, rows, r); Add(xl,x); Dec(xr, x)
    y = DivY(i, rows, r); Add(yl,y); Dec(yr, y)
    if (i.step > hi-r) break
    if (i.step > lo+r) continue
    after = DivX(i, rows, r+1)
    if (x == after) continue 
    if (i.epsilon > x - i.start ) continue
    if (i.epsilon > i.stop - after  ) continue
    if (i.epsilon > Mid(xr) - Mid(xl) ) continue
    if (i.trivial * Xpect(yl,yr) >= min)  continue
    min = Xpect(yl,yr)
    cut = r
    copy(xr, xr1); copy(xl, xl1)
    copy(yr, yr1); copy(yl, yl1) 
  }
  return cut
}
BEGIN {rogues()}
