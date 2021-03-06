# vim: nospell filetype=awk ts=2 sw=2 sts=2  et :
#--------- --------- --------- --------- --------- ---------

BEGIN{ DOT=sprintf("%c",46)}

function my(i,f,    k,m) {
  k = i["isa"]
  while(k) {
    m=k f
    if (m in FUNCTAB) return m
    k=ISA[k]
  }
  print "E> failed method lookup on ["f"]"; 
  exit 1
}

function isa(i,x,y,_) { i["isa"]=x; ISA[x]=y }

function new(i) { split("",i,"") }
function Object(i)   { new(i); i["isa"]="Object"; i["oid"] = ++OID }

function warning(txt) {
  print "#E> " txt > "/dev/stderr"
}
function error(txt) {
  warning(txt)
  fflush("/dev/stderr")
  exit 1
}

function tests(what, all,   one,a,i,n) {
   n = split(all,a,",")
   print " "
   print "#--- " what " -----------------------"
   for(i=1;i<=n;i++) {
     one = a[i]
     @one(one) }
   rogues()
}
function is(f,got,want,    pre) {
  if (want == "") want=1
  if (want == got)
    pre="#TEST:\tPASSED"
  else
    pre="#TEST:\tFAILED"
  print( pre "\t" f "\t" want "\t" got )
}

function rogues(    s) {
  for(s in SYMTAB) 
    if (s ~ /^[A-Z][a-z]/) print "Global " s
  for(s in SYMTAB) 
    if (s ~ /^[_a-z]/) print "Rogue: " s
}


function has(lst,key,fun) {
  lst[key][SUBSEP]
  split("",lst[key],"")
  if (fun) @fun(lst[key])
}
function have( a,k,f,b)           { has(a,k); @f(a[k],b) }
function haves(a,k,f,b,c)         { has(a,k); @f(a[k],b,c) }
function haveS(a,k,f,b,c,d)       { has(a,k); @f(a[k],b,c,d) }
function havES(a,k,f,b,c,d,e)     { has(a,k); @f(a[k],b,c,d,e) }
function haVES(a,k,f,b,c,d,e,g)   { has(a,k); @f(a[k],b,c,d,e,g) }
function hAVES(a,k,f,b,c,d,e,g,h) { has(a,k); @f(a[k],b,c,d,e,g,h) }


# text stuff
function fyi(x) { print x >> "/dev/stderr" }
