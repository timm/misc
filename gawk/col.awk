#!/usr/bin/env ./gold
# vim: filetype=awk ts=2 sw=2 sts=2  et :

@include "my"
@include "lib"

func Col(i,txt,pos) {
  is(i,"Col")
  i.n     = 0
  i.txt   = txt
  i.pos   = pos
  i.goal  = txt ~ MY.goal
  i.klass = txt ~ MY.klass
  i.w     = txt ~ MY.less ? -1 : 1
}
func all(a,how,    v,j) {
  if (!isarray(how))
    typeof(a[1])== "number" ? Num(how) :  Sym(how);
  for(j in a) {
    v = a[j]
    if (v == MY.skip) next
    Add(how, v) }
}
func _all(    a,how,j) {
  for(j=1;j<=10;j++) a[j]=j
  all(a,how)
  oo(how)
}
