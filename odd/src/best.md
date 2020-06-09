<a name=top>
<h1 align=center><a href="https://github.com/timm/README.md#top">SH APE (SHaring A Programmer Environment)</a></h1>
<p align=center> <a
href="https://github.com/aiez/eg/blob/master/LICENSE">license</a> :: <a
href="https://github.com/aiez/eg/blob/master/INSTALL.md#top">install</a> :: <a
href="https://github.com/aiez/eg/blob/master/CODE_OF_CONDUCT.md#top">contribute</a> :: <a
href="https://github.com/aiez/eg/issues">issues</a> :: <a
href="https://github.com/aiez/eg/blob/master/CITATION.md#top">cite</a> :: <a
href="https://github.com/aiez/eg/blob/master/CONTACT.md#top">contact</a> </p><p align=center>
<img width=600 src="https://github.com/timm/misc/blob/master/odd/etc/img/solidgallery.gif"></p><p 
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
@include "table"

function best(i,min,out,   rows) {
  for(r in i.data) 
    if (rand() < the.best.want/length(data)) 
      push(rows,r)
  besthalves(i,rows, out, 2*length(data)^the.best.min)
}

function besthalves(i,rows,out,min,  x,tmp) {
  if (length(rows) < min)  {
    for(x in rows)
      out[x] = rows[x] 
  } else { 
     besthalf(  i,rows,tmp)
     besthalves(i,tmp, out, min) }
}
      
function besthalf(i,rows,out,
                  one,two,three,c,r,a,b,c,x,mid,d,left) {
  one     = any(rows)
  two     = distant(i,one,rows,   i.my.goals)
  three   = distant(i,two,rows,   i.my.goals)
  c       = dist(i,   two, three, i.my.goals)
  for(r in rows) {
    a     = dist(r, two)
    b     = dist(r, three)
    x     = (a^2+c^2 - b^2) / (2*c) 
    if (x > 1) x = 1
    if (x < 0) x = 0
    mid   += x/length(rows)
    d[r]  = x
  }
  left = dom(i,two,three) 
  for(r in d) 
    if left  {
      if (d[r] <= mid) push(out,r)
    } else
      if (d[r] >= min) push(out,r)
}
function distant(i,r1,rows,cols,  a,n) {
  for(r2 in rows) 
    if(r1 != r2) {
      a[r2].row = r2
      a[r2].dist = dist(i,r1,r2,cols) }
  n = keysort(a,"dist")
  n = int(n*the.distant.far)  
  return a[n].row
}
function dom(i,r1,r2,   e,n,x,y,s1,s2) {   
  n = length(i.my.goals)
  for(c in i.my.goals) {
    x   = i.data[r1][c]
    y   = i.data[r2][c]
    x   = norm(i, c, x)
    y   = norm(i, c, y)
    s1 -= 2.72 ^ ( i.cols[c].w * (x - y)/n )
    s2 -= 2.72 ^ ( i.cols[c].w * (y - x)/n )
  }
 return s1/n < s2/n
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
