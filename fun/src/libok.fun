#!/usr/bin/env ./fun
# vim: filetype=awk ts=2 sw=2 sts=2  et :

@include "lib"

function Demo(i) {
  i.aaa = 10
  i.bbb = "no"
  i.ccc = "where" DOT "csv"
}
BEGIN {
  Demo(i)
  argv(i)
  oo(i)
  print(typeof(i.aaa))
  print(typeof(i.bbb))
}
