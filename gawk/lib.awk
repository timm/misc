#!/usr/bin/env ./gold
# vim: filetype=awk ts=2 sw=2 sts=2  et :

@include "object"
@include "test"

BEGIN {AU.dot = sprintf("%c",46) }

func copy(a,b,   j) { for(j in a) b[j] = a[j] }
func push(a,x)      { a[length(a)+1] = x; return x}

func last(a) { return a[ length(a) ] }

func o(a,   s,sep,j) {
  s=sep=""
  for(j in a) {
    s= s sep a[j]
    sep=", "
  }
  print s
}
func oo(x,p,pre,      j,txt) {
  txt = pre ? pre : (p AU.dot)
  ooSortOrder(x)
  for(j in x)  {
    if (isarray(x[j]))   {
      print(txt j"" )
      oo(x[j],"","|  " pre)
    } else
      print(txt j (x[j]==""?"": ": " x[j])) }
}
func ooSortOrder(x, j) {
  for (j in x)
    return PROCINFO["sorted_in"] = \
      typeof(j + 1)=="number" ? "@ind_num_asc" : "@ind_str_asc" 
}
func csv(f,a,     b4, g,txt) {
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
