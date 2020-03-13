#!/usr/bin/env ../fun
# vim: filetype=awk ts=2 sw=2 sts=2  et :

@include "lib.fun"
______________________________

function Some0(i) {
  if (!i.magic) {
    List(i)
    i.magic = FUN.some.magic # 2.56
    i.max   = FUN.some.max   # 256
    i.small = FUN.stats.cliffs.small} # 0.147
}
function Some(i,pos) {
  Some0(i)
  has(i,"has")
  has(i,"cuts")
  i.pos    = pos ? pos : 1
  i.sorted = 0
  i.n      = 0 
}
function Some1(i,x) {
  if (x == "?") return
  i.n++
  if (i.n < i.max) {
    i.has[ l(i.has)+1 ] = x
    i.sorted=0
  } else {
    if (i.n == i.max) 
      i.sorted = asort(i.has)
    if (rand() < i.max/i.n)
      i.has[ binChop(i.has,x) ] = x }
}
function SomeDiff(a,b,  
                  la,lb,j,x,lo,hi,gt,lt) {
  la = sorted(a)
  lb = sorted(b)
  for(j in a.has) {
    x  = a.has[j]
    lo = hi= binChop(b.has, x)
    while(lo >= 1 && b.has[lo] == x) lo--
    while(hi <= n && b.has[hi] == x) hi++
    gt += lb - hi 
    lt += lo
  }
  return abs(lt-gt)/(la*lb) > a.small 
}

function sorted(i)  { 
  if (!i.sorted) 
    i.sorted=asort(i.has) 
  return length(i.has)
}
function at(i,z)      { if(!i.sorted) sorted(i);  return i.has[int(z)] }
function per(i,j,k,p) { return at(i,j + p*(k-j))   }
function mid(i,j,k)   { return at(i,j + .5*(k-j) ) }
function sd(i,j,k)    {
  return abs(per(i,j,k,.9) - per(i,j,k,.1))/i.magic 
}
function xpect(i,j,m,k,   n) {
  n=k-j+1
  return (m-j)/n*sd(i,j,m) + (k-m -1)/n*sd(i,m+1,k) 
}

BEGIN { Some(i); argv(i); oo(i)}
