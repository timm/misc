<a name=top>
<h1 align=center><a href="/README.md#top">APE = another programmer's environment</a></h1>
<p align=center> <a
href="https://github.com/aiez/eg/blob/master/LICENSE">license</a> :: <a
href="https://github.com/aiez/eg/blob/master/INSTALL.md#top">install</a> :: <a
href="https://github.com/aiez/eg/blob/master/CODE_OF_CONDUCT.md#top">contribute</a> :: <a
href="https://github.com/aiez/eg/issues">issues</a> :: <a
href="https://github.com/aiez/eg/blob/master/CITATION.md#top">cite</a> :: <a
href="https://github.com/aiez/eg/blob/master/CONTACT.md#top">contact</a> </p><p align=center>
<img width=300 src="img/monkey.png"></p><p 
align=center><img
src="https://img.shields.io/badge/language-lua-orange"> <img
src="https://img.shields.io/badge/purpose-ai,se-blueviolet"> <img
src="https://img.shields.io/badge/platform-mac,*nux-informational"><a
     href="https://travis-ci.org/github/sehero/lua"> <img
src="https://travis-ci.org/aiez/eg.svg?branch=master"></a><a
     href="https://zenodo.org/badge/latestdoi/263210595"> <img
src="https://zenodo.org/badge/263210595.svg" alt="DOI"></a><a
     href='https://coveralls.io/github/aiez/lua?branch=master'> <img i
src='https://coveralls.io/repos/github/aiez/eg/badge.svg?branch=master' 
alt='Coverage Status' /></a></p>

# best

```awk
@include "lib"

#define Data head,w,lo,hi,data

function read(f,Data,    r,c) {
  FS = ","
  f  = f ? f : "-"
  while((getline f) > 0)  { 
    gsub(/[ \t\r]*/, "")
    if(r++ == 0)
      for(c=1;c<=NF;c++)  
        readHeader(c,$c,Data)
    else
      for(c=1;c<=NF;c++)  
        readCell(r,c,$c,Data) }
}
function readHeader(c,x,Data) {
  head[c] = x
  if (x ~ /</)  w[c] = -1 
  if (x ~ />/)  w[c] =  1 
  if (x ~ /[<>\$]/) {
     lo[c] =  10^32
     hi[c] = -10^32 }
}
function readCell(r,c,x,Data) {
  if (x ~ /\?/) return x
  if (c in lo) { # for all nums do
    if (x > hi[c]) hi[c] = x
    if (x < lo[c]) lo[c] = x 
  }
  data[r][c] = x
}

```
```awk
function distant(r1,Data,  a,n) {
  for(r2 in data) 
    if(r1 != r2) {
      a[r2].row = r2
      a[r2].dist = dist(r1,r2,Data) }
  n = keysort(a,"dist")
  n = int(n*the.distant.far)  
  return a[n].row
}
function dist(r1,r2,Data,  x,y,d,n) {
  n = 0.00001 # stop divide by zero errors
  for(c in w) {
    x  = norm(c, data[r1][c], Data)
    y  = norm(c, data[r2][c], Data)
    d += abs(x-y)^2
    n++
  }
  return (d/n)^0.5
}
function norm(c,x,Data) {
  if (x ~ /\?/) return x
  return (x - lo[c])/(hi[c] - lo[c] + 10^-32)
}
```

## Main

```awk
function main(     head,w,lo,hi,data) {
  read(head,w,lo,hi,data)
  oo(data,"d")
}
BEGIN { 
  rogues() 
}
```
