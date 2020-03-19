#!/usr/bin/env ./gold
# vim: filetype=awk ts=2 sw=2 sts=2  et :

@include "my"
@include "lib"
@include "col"

function Sym(i,txt,pos) { 
  isass(i,"Sym","Col",txt,pos)
  i.mode=""
  i.most=0
  has(i,"seen") 
}
function SymVar(i) { return SymEnt(i) }
function SymMid(i) { return i.mode  }

function SymAdd(i,v,  tmp) {
  if (v ~ MY.skip) return v
  i.n++
  tmp = ++i.seen[v]
  if (tmp > i.most) { i.most = tmp; i.mode = v }
  return v 
}
function SymDec(i,v) {
  if (v ~ MY.skip) return v
  if (v in i.seen && i.seen[v] > 0) { 
    i.n--
    i.seen[v]-- }
}
function SymEnt(i,   p,e,k) {
  for(k in i.seen) {
    p  = i.seen[k]/i.n
    e -= p*log(p)/log(2)
  }
  return e
}  
