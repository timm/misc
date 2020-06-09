<a name=top>
<h1 align=center><a href="/README.md#top">SH APE (SHaring A Programmer Environment)</a></h1>
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

# Num

Create

```awk
function Num(i,txt,pos) {
  Object(i)
  is(i, "Num")
  i.pos = pos
  i.txt = txt
  i.w   = (txt ~ /</) ? -1 : 1
  i.n   = i.mu = i.m2 = i.sd = 0
  i.lo  = 10^32
  i.hi  = -1*i.lo
}
```

Updates

```awk
function NumAdd(i,v,    d) {
  if (v=="?") return v
  v += 0
  i.n++
  i.lo  = v < i.lo ? v : i.lo
  i.hi  = v > i.hi ? v : i.hi
  d     = v - i.mu
  i.mu += d/i.n
  i.m2 += d*(v - i.mu)
  NumSd(i)
  return v
}
function NumSd(i) {
  if (i.m2 < 0) return 0
  if (i.n  < 2) return 0
  i.sd = (i.m2/(i.n - 1))^0.5
  return i.sd
}
```

Discretization (cut two Gaussians, four ways).

```awk
function Num4Cuts(i,j,x,    a,b,c,d) {
  a = 1/(2*i.sd^2)      - 1/(2*j.sd^2)
  b = j.mu/(j.sd^2)     - i.mu/(i.sd^2)
  c = i.mu^2/(2*i.sd^2) - j.mu^2/(2*j.sd^2) - log(j.sd/i.sd)
  d = b^2 - 4 * a * c
  x[1]= (-b - sqrt(d))/(2*a)
  x[2]= i.mu
  x[3]= (-b + sqrt(d))/(2*a)
  x[4]= j.mu
  asort(x)
}
```
