##- vim: ft=awk ts=2 sw=2 et :

BEGIN { print 1 }

function Num(i) { 
   i["lo"]=10
   i["hi"]=10
}
function header(  i) { print "h" }
function data(  i) { print "d" }

function reads(f,    n) {
  f = f ? f : "-"
  while ((getline < f) > 0)
    n++ ? data() : header()  
  print n
}

BEGIN  { print Gold["dot"] }
#BEGIN {reads("data/weathr["csv"]"") }

