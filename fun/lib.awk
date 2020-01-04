# vim: nospell filetype=awk ts=2 sw=2 sts=2  et :

## Misc utilities.

BEGIN{  DOT="." }

function l(a) { return length(a) }

function o(a,t   ,x,s,sep) {
  for(x in a) {
    s= s sep  t"["x"]="a[x]; sep=", " }
  return s
}

function rogues(    s) {
  for(s in SYMTAB) {
     if (s ~ /^[A-Z][a-z]/) print "Global " s
     if (s ~ /^[_a-z]/    ) print "Rogue: " s }
}

function List(i)         { split("",i,"") }
function zap(i,k)        { i[k][0]; List(i[k])} 
function Object(i)       { List(i); i["oid"]=++OID }

function has( i,k,f)     { f=f?f:"List"; zap(i,k); @f(i[k]) }
function hass(i,k,f,m)   {               zap(i,k); @f(i[k],m) }
function hasss(i,k,f,m,n) {              zap(i,k); @f(i[k],m,n) }

 function lines(i,f, update,sep,  r,line,lst) {
  f   = f ? f : "/dev/stdin"
  sep = sep ? sep : "[ \t]*,[ \t]*"
  while((getline line < f) > 0) {
    gsub(/^[ \t\r]*/,"",line)
    gsub(/[ \t\r]*$/,"",line)
    if (line) { 
      split(line,lst,sep)
      @update(i,++r,lst) }}
  close(f) } 

function abs(x) {return x<0? -1*x : x }

function binChop(a,x,           y,lo, hi,mid)  {
  lo = 1
  hi = l(a)
  while (lo <= hi) {
    mid = int((hi + lo) / 2)
    y=a[mid]
    if (x == y) break
    if (x <  y) hi=mid-1; else lo=mid+1 }
  return mid 
}

function oo(x,p,pre, i,txt) {
  txt = pre ? pre : (p DOT)
  ooSortOrder(x)
  for(i in x)  {
    if (isarray(x[i]))   {
      print(txt i"" )
      oo(x[i],"","|  " pre)
    } else
      print(txt i (x[i]==""?"": ": " x[i]))
}}
function ooSortOrder(x, i) {
  for (i in x)
    return PROCINFO["sorted_in"] =\
      typeof(i + 1)=="number" ? "@ind_num_asc" : "@ind_str_asc" }

function argv(b4,   x,k,v,w) {
  for (x in ARGV)  {
   k = ARGV[x]
   if (sub(/^--/,"",k))  
     if (k in b4) 
      if (typeof(b4[k]) =="number")
        b4[k] = strtonum(ARGV[x+1])
      else
        b4[k] = ARGV[x+1]}
}

function cat(a,sep,    j,n,s) {
  sep = sep  ? sep : ","
  n = l(a)
  s = a[1]
  for(j=2;j<=n;j++) s = s sep a[j]
  return s }

function ksort(lst,k) { 
  SORT=k; return asort(lst,lst,"kcompare") }

function kcompare(i1,v1,i2,v2,  l,r) {
  l = v1[SORT] +0
  r = v2[SORT] +0
  if (l < r) return -1
  if (l == r) return 0
  return 1 } 

function cellsort(lst,k) { 
  SORT=k; return asort(lst,lst,"cellcompare") }

function cellcompare(i1,v1,i2,v2,  l,r) {
  l = v1["cells"][SORT]
  r = v2["cells"][SORT]
  if (l < r) return -1
  if (l == r) return 0
  return 1 }  

