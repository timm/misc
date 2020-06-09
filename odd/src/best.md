<a name=top>
<h1 align=center><a href="/README.md#top">sh ape = <u>sh</u>aring <u>a</u>  <u>p</u>rogrammer <u>e</u>nvironment</a></h1>
<p align=center> <a
href="https://github.com/aiez/eg/blob/master/LICENSE">license</a> :: <a
href="https://github.com/aiez/eg/blob/master/INSTALL.md#top">install</a> :: <a
href="https://github.com/aiez/eg/blob/master/CODE_OF_CONDUCT.md#top">contribute</a> :: <a
href="https://github.com/aiez/eg/issues">issues</a> :: <a
href="https://github.com/aiez/eg/blob/master/CITATION.md#top">cite</a> :: <a
href="https://github.com/aiez/eg/blob/master/CONTACT.md#top">contact</a> </p><p align=center>
<img width=400 src="https://github.com/timm/misc/blob/master/odd/etc/img/monkey.png"></p><p 
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
@include "ape"
@include "num"
@include "sym"

#define Data head,w,lo,hi,data

function add(i,x, f) { f=i.is "Add"; return @f(i,x) }

function Table(i) {
  Object(i)
  is(i,"Table")
  has(i,"cols")
  has(i,"rows")
}
function what(s) { return s ~ /[\$<>]/ ? "Num" : "Sym" }

function read(i,f,    f,r,c) {
  FS = ","
  f  = f ? f : "-"
  r  = -1
  while((getline f) > 0)  { 
    gsub(/[ \t\r]*/, "")
    if(++r == 0) {
      for(c=1; c<=NF; c++) 
        if ($c !~ /\?/)
          hass(i.cols, c, what($c), $c, c) 
    } else
        for(c in i.cols)
          i.rows[r][c] = add(i.cols[c], $c)
}
```

```awk
function distant(i,r1,  a,n) {
  for(r2 in data) 
    if(r1 != r2) {
      a[r2].row = r2
      a[r2].dist = dist(i,r1,r2) }
  n = keysort(a,"dist")
  n = int(n*the.distant.far)  
  return a[n].row
}
function dist(i,r1,r2,  x,y,d,n) {
  n = 0.00001 # stop divide by zero errors
  for(c in w) {
    x  = norm(i, c, data[r1][c])
    y  = norm(i, c, data[r2][c])
    d += abs(x-y)^2
    n++
  }
  return (d/n)^0.5
}
function norm(i,c,x,   lo,hi) {
  if (x ~ /\?/) return x
  lo = i.cols[c].lo
  hi = i.cols[c].hi
  return (x - lo)/(hi - lo + 10^-32)
}
function dom(i,r1,r2,   
                 e,n,col,x,y,sum1,sum2) {
  # Idom= indicator domination
  e = 2.71828
  n = length(t.dom)
  for(col in t.dom) {
    x     = norm(t.nums[col], i.cells[col])
    y     = NumNorm(t.nums[col], j.cells[col])
    s1 -= e ^ ( t.dom[col] * (x - y)/n )
    s2 -= e ^ ( t.dom[col] * (y - x)/n )
  }
 return s1/n < s2/n
}

```

```awk
function fastmap1(Data  all,r) {
  for(r in data) some[r]=r
  fastmap1(Data,some)
}
 
function fastmap1(Data,all,left,right,
                  one,two,three,c,r,a,b,c,x,xs,x1) {
  one     = any(all)
  two     = distant(one, Data)
  three   = distant(two, Data)
  c       = dist(   two, three, Data)
  for(r in all) {
    a     = dist(r, two)
    b     = dist(r, three)
    x     = (a^2+c^2 - b^2) / (2*c) 
    if (x > 1) x = 1
    if (x < 0) x = 0
    xs   += x
    at[r] = x
  }
  xs = xs / length(at) # halfway
  for(r in at) 
    at[r] < xs ? push(left,r) : push(right,r)
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
