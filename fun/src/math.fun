#!/usr/bin/env ../fun
# vim: filetype=awk nospell ts=2 sw=2 sts=2  et :

function min(x,y) { return x<y?    x : y }
function max(x,y) { return x>y?    x : y }
function abs(x)   { return x<0? -1*x : x }

function gauss(m,s) { return m + s*z() }
function z() { 
  return sqrt(-2*log(rand()))*cos(6.2831853*rand())
}
