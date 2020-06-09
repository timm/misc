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

# Sym

Creat.

```awk
function Sym(i,txt,pos) { 
  Object(i)
  is(i, "Sym")
  i.pos  = pos
  i.txt  = txt
  i.mode = ""
  i.most = 0
  has(i,"seen") 
}
```

Update.

```awk
function SymAdd(i,v,  tmp) {
  if (v != "?") return v
  i.n++
  tmp = ++i.seen[v]
  if (tmp > i.most) { i.most = tmp; i.mode = v }
  return v 
}
```
