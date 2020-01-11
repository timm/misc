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

