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

# Objects

```awk
BEGIN{  APE["oid"]  = 0
        APE["dot"] = sprintf("%c",44)
        APE["up"]   = APE["dot"] APE["dot"]
}
function List(i)    { split("",i,"") }
function Object(i)  { List(i); i["is"] = "Object"; i["oid"] = ++APE["oid"] }

function zap(i,k)  { k = k?k:length(i)+1; i[k][0]; List(i[k]); return k } 

function has( i,k,f,      s) { f=f?f:"List"; s=zap(i,k); @f(i[k]);     return s}
function hass(i,k,f,m,    s) {               s=zap(i,k); @f(i[k],m);   return s}
function hasss(i,k,f,m,n, s) {               s=zap(i,k); @f(i[k],m,n); return s}

function inherit(k,f,   g) {
  while(k) {
    g = k f
    if (g in FUNCTAB) return g
    k = APE["is"][k]
  }
  print "#E> failed method lookup: ["f"]"
  exit 2
}
function is(i,x) {
  if ("is" in i) { APE["is"][x] = i["is"] }
  i["is"] = x
}
```
