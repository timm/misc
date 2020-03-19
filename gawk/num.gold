#!/usr/bin/env ./gold
# vim: filetype=awk ts=2 sw=2 sts=2  et :

@include "my"
@include "lib"
@include "col"

func Num(i,txt,pos) { 
  isass(i,"Num","Col",txt,pos)
  i.mu = i.m2 = i.sd = 0
  i.lo = 10^32 
  i.hi = -1*i.lo 
}
func NumVar(i) { return i.sd }
func NumMid(i) { return i.mu }

function NumAdd(i,v,    d) {
  if (v ~ MY.skip) return v
  v += 0
  i.n++
  i.lo  = v < i.lo ? v : i.lo
  i.hi  = v > i.hi ? v : i.hi
  d     = v - i.mu
  i.mu += d/i.n
  i.m2 += d*(v - i.mu)
  i.sd  = NumSd(i)
  return v 
}
func NumSd(i) {
  if (i.m2 < 0) return 0
  if (i.n < 2)  return 0
  return  (i.m2/(i.n - 1))^0.5 
}
func NumDec(i,v, d) {
  if (i.n < 2)  return v
  if (v ~ MY.skip) return v
  i.n  -= 1
  d     = v - i.mu
  i.mu -= d/i.n
  i.m2 -= d*(v - i.mu)
  i.sd  = NumSd(i) 
  return v
}
