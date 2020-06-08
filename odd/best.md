# best

```awk
@include "lib"

function head1(c,x,head,w,lo,hi) {
  head[c] = x
  if (x ~ /</)  w[c] = -1 
  if (x ~ />/)  w[c] =  1 
  if (x ~ /[<>\$]/) {
     lo[c] =  10^32
     hi[c] = -10^32 }
}
function cell1(r,c,x,lo,hi,data) {
  if (x ~ /\?/) return x
  if (c in lo) {
    if (x > hi[c]) hi[c] = x
    if (x < lo[c]) lo[c] = x 
  }
  data[r][c] = x
}
function norm(c,x,lo,hi) {
  if (x ~ /\?/) return x
  return (x - lo[c])/(hi[c] - lo[c] + 10^-32)
}
function distant(r1,data,lo,hi) {
  for(r2 in data) 
    if(r1 != r2)
     
}
function dist(r1,r2,data,lo,hi,w) {
  for(c in w) {
    x= data[r1][c] 
    y= data[r2][c] 
  }
}
function read(head,w,lo,hi,data,f,    r,c) {
  FS = ","
  f  = f ? f : "-"
  while((getline f) > 0)  {
    gsub(/[ \t\r]*/, "")
    if(r++ == 0)
      for(c=1;c<=NF;c++)  
        head1(c,$c,head,w,lo,hi)
    else
      for(c=1;c<=NF;c++)  
        cell1(r,c,$c,lo,hi,data) }
}
```

```awk
function main(     head,w,lo,hi,data) {
  read(head,w,lo,hi,data)
  oo(data,"d")
}
BEGIN { 
  rogues() 
}
```
