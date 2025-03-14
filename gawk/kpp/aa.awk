#!/usr/bin/env ./gold
# vim: filetype=awk ts=2 sw=2 sts=2  et :

@include "object"
@include "test"
@include "my"

BEGIN {AU["dot"] = sprintf("%c",46) }

function keysort(a,k) {
  AU["keysort"] = k
  return asort(a,a,"__keysort")
}
function __keysort(i1,x,i2,y) {
  return compare(x[ AU["keysort"] ] + 0,
                 y[ AU["keysort"] ] + 0)
} 
function copy(a,b,   j) { 
  for(j in a) 
    if(isarray(a[j]))  {
      has(b,j)
      copy(a[j], b[j]) 
    } else
      b[j] = a[j] 
}
function push(a,x) { a[length(a)+1] = x; return x}

function compare(x,y) {
  if (x < y) return -1
  if (x == y) return 0
  return 1
}

function _(a)      { split("",a,"") } 
function abs(x)    { return x > 0 ? x : -1*x }
function min(x,y)  { return x < y ? x : y    }
function max(x,y)  { return x < y ? y : x    }

function first(a)  { return a[1] }
function last(a)   { return a[length(a)] }
function anyi(a)   { return 1+int(rand() * length(a)) }
function any(a)    { return a[ anyi(a) ] }

function shuffle(a,  i,j,n,tmp) {
  n=length(a)
  for(i=1;i<=n;i++) {
    j=i+round(rand()*(n-i));
    tmp=a[j];
    a[j]=a[i];
    a[i]=tmp;
  };
  return n;
}
function int2bitStr(bits,        data, mask) {
  if (bits == 0) return "0"
  mask = 1
  for (; bits != 0; bits = rshift(bits, 1))
    data = (and(bits, mask) ? "1" : "0") data
  while ((length(data) % 8) != 0)
    data = "0" data
  return data
}
function ns(n,s,  t) {
  s = s ? s  : "*" 
  while(n-- > 0) t = t s
  return t
}
function binChop(a,x,           y,lo, hi,mid)  {
  lo = 1
  hi = length(a)
  while (lo <= hi) {
    mid = int((hi + lo) / 2)
    y = a[mid]
    if (x == y) break
    if (x <  y) {hi=mid-1} else {lo=mid+1} 
  }
  return mid 
}
function o(a,   s,sep,j) {
  s=sep=""
  for(j in a) {
    s= s sep a[j]
    sep=", "
  }
  print s
}
function oo(x,p,pre,      j,txt) {
  txt = pre ? pre : (p AU["dot"])
  ooSortOrder(x)
  for(j in x)  
    if (j !~ /^_/) {
      if (isarray(x[j]))   {
        print(txt j"" )
        oo(x[j],"","|  " pre)
      } else
        print(txt j (x[j]==""?"": ": " x[j])) }
}
function ooSortOrder(x, j) {
  for (j in x)
    return PROCINFO["sorted_in"] = \
      typeof(j + 1)=="number" ? "@ind_num_asc" : "@ind_str_asc" 
}
function csv(f,a,     b4, g,txt) {
  f = f ? f : "-"             
  g = getline < f
  if (g< 0) { print "#E> Missing f ["f"]"; exit 1 } # file missing
  if (g==0) { close(f) ; return 0 }       # end of file                   
  txt = b4 $0                             # combine with prior
  gsub(/[ \t]+/,"",txt)
  if (txt ~ /,$/) { return csv(f,a,txt) } # continue txt into next
  sub(/#.*/, "", txt)                    # kill whitespace,comments    
  if (!txt)       { return csv(f,a,txt) } # skip blanks
  split(txt, a, ",")                      # split on "," into "a"
  return 1
}

