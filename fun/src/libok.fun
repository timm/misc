#!/usr/bin/env ../fun
# vim: filetype=awk ts=2 sw=2 sts=2  et :

@include "lib.fun"

function Demo(i) {
  Object(i)
  i.aaa = 10
  i.bbb = "no"
  i.ccc = "where" FUN.dot "csv"
}
function libargs(w,   i,j) {
  Demo(i)
  argv(i)
  for(j in ARGV) print j, ARGV[j]
  oo(i)
  is(w, typeof(i.aaa),"number")
  is(w, typeof(i.bbb),"string")
}
function libokMain() { 
  tests("libok","libargs") 
}
