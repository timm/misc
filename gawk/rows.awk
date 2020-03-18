#!/usr/bin/env ./gold
# vim: filetype=awk ts=2 sw=2 sts=2  et :

@include "my"
@include "lib"
@include "num"
@include "sym"
@include "row"

func Rows(i) {
  is(i,"Rows")
  has(i,"rows")
  has(i,"cols")
  i.klass = ""
}
func RowsEmpty(i) { 
  return length(i.cols) == 0 
}
func RowsUses(i,a,use,       n,j) {
  for(j=1;j<=length(a);j++) 
    if(a[j] ~ AU.skip)
      use[++n]=j
}
func RowsAddCols(i,a,       c) { 
  for(c in a) 
    hasss(i.cols, c, a[c] ~ MY.numeric ? "Num" : "Sym", a[c],  c)
}
func RowsAdd(i,a,  r,c,x) {
  r = length(i.rows) + 1
  has(i.rows, r, "Row")
  for(c in a) 
    i.rows[r].cells[c] = Add(i.cols[c], a[c])
}
func RowsRead(i,file,    a,b,use,j) {
  while(csv(file,a)) {
    if (RowsEmpty(i))
      RowsUses(i,a,use);
    for(j in use) 
      b[j] = a[ use[j] ];
    RowsEmpty(i) ? RowsAddCols(i,b) :  RowsAdd(i,b) }
}   
func _rows(    i,d) {
  d=AU.dot
  Rows(i);  RowsRead(i, d d "/data/weather" d "csv")
  print "d " d
  oo(i)
}
