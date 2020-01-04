#!/usr/bin/env ./fun
# vim: filetype=awk ts=2 sw=2 sts=2  et :

@include "lib"

______________________________
function Some(i,pos) {
  Object(i)
  has(i,"has")
  has(i,"cuts")
  i.pos    = pos ? pos : 1
  i.magic  = 2.56
  i.max    = 256
  i.small   = 0.147
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
function sorted(i)  { 
  if (!i.sorted) 
    i.sorted=asort(i.has) 
  return length(i.has)
}
function at(i,z)      { sorted(i);  return i.has[int(z)] }
function per(i,j,k,p) { return at(i,j + p*(k-j))   }
function mid(i,j,k)   { return at(i,j + .5*(k-j) ) }
function sd(i,j,k)    {
  return abs(per(i,j,k,.9) - per(i,j,k,.1))/i.magic 
}
function xpect(i,j,m,k,   n) {
  n=k-j+1
  return (m-j)/n*sd(i,j,m) + (k-m -1)/n*sd(i,m+1,k) 
}

function SomeDiff(i,j,   k,la,lb,n,x,lo,hi,gt,lt) {
  # Returns 1 if i,j differ by more than a small effect
  la = sorted(i)
  lb = sorted(j)
  for(k in i.has) {
    x= i.has[k]
    lo= hi= binChop(j.has, x)
    while(lo > 1 && i.has[lo] >= x) lo--
    while(hi < n && i.has[hi] <= x) hi++
    lt += la - hi + 1
    gt += lo
  }
  return i.small < abs(gt - lt) / (la*lb)  
}

