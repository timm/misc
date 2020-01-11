#!/usr/bin/env ./fun
# vim: filetype=awk ts=2 sw=2 sts=2  et :

function Table0(i) {
  i.data = "data/weather" DOT "csv"
  i.sep  = ","
}
function Table(i) {
  Object(i)
  has(i,"rows")
  has(i,"names")
  has(i,"nums") 
}
function TableRead(i,f) { lines(i,f, "Table1") }

function Table1(i,r,lst,      c,x) {
  if (r>1)  
    return hasss(i.rows,r-1,"Row",lst,i)
  for(c in lst)  {
    x = i.names[c] = lst[c]
    if (x ~ /[\$<>]/) 
      hass(i.nums,c,"Some",c) }
}
function TableDump(i,   r) {
  print(cat(i.names))
  for(r in i.rows)
    print(cat(i.rows[r].cells)) 
}
_______________________________
function Row(i,lst,t,     x,c) {
  Object(i)
  has(i,"cells")
  for(c in t.names) {
    x = lst[c]
    if (x != "?") {
      if (c in t.nums) {
         x += 0
         Some1(t.nums[c], x) }
      i.cells[c] = x }}
}
