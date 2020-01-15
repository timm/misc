#!/usr/bin/env gawk -f
# vim: nospell ts=2 sw=2 sts=2  et :

function lines(i,f, update,sep,  r,line,lst) {
  f   = f ? f : "/dev/stdin"
  sep = sep ? sep : "[ \t]*,[ \t]*"
  while((getline line < f) > 0) {
    gsub(/^[ \t\r]*/,"",line)
    gsub(/[ \t\r]*$/,"",line)
    if (line) { 
      split(line,lst,sep)
      @update(i,++r,lst) }}
  close(f) 
} 

function argv(b4,   x,k,old,new) {
  for (x in ARGV)  {
   k = ARGV[x]
   if (sub(/^--/,"",k))  
     b4[k] = argv1(k, k in b4, b4[k], ARGV[x+1]) }
}

function argv1(k,known,old,new) {
  if (known) {
    if (typeof(old) != "number")
      return new
    else if (new ~ /[0-9]+(\.[0-9]*)?/)
      return strtonum(new)   }
  print "#W> bad arg --"k" " new
  exit 1 
}

