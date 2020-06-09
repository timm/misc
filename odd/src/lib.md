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

# Lib

## Maths

```awk
function abs(x)  { return x>=0? x: -1*x }
```
## Lists

Misc

```awk
function any(a)    { return int(0.5 + rand()*length(a)) }
function push(a,x) { a[length(a)+1] = x; return x }
```

Display nested lists.

```awk
function oo(a,prefix,    indent,   i,txt) {
  txt = indent ? indent : (prefix AU["dot"] )
  ooSortOrder(a)
  for(i in a)  {
    if (isarray(a[i]))   {
      print(txt i"" )
      oo(a[i],"","|  " indent)
    } else
      print(txt i (a[i]==""?"": ": " a[i])) }
}
function ooSortOrder(a, i) {
  for (i in a)
   return PROCINFO["sorted_in"] =\
     typeof(i+1)=="number" ? "@ind_num_asc" : "@ind_str_asc"
}
```

Sorting

```awk
function keysort(a,k) {
  APE.keysort = k
  return asort(a,a,"keysorter")
}
function keysorter(i1,x,i2,y) {
  return compare(x[ APE.keysort ] + 0,
                 y[ APE.keysort ] + 0)
} 
function compare(x,y) {
  if (x < y) return -1
  if (x == y) return 0
  return 1
}
```

## Debugging

```awk
function rogues(    s) {
  for(s in SYMTAB) 
    if (s ~ /^[A-Z][a-z]/) 
      print "#W> Global " s>"/dev/stderr"
  for(s in SYMTAB) 
    if (s ~ /^[_a-z]/    ) 
      print "#W> Rogue: " s>"/dev/stderr"
}
```
