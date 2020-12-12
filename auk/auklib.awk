BEGIN {
   Gold["dot"] = sprintf("%c",46) 
   Gold["id"] = 0 }

function List(i)      { split("",i,"");  }
function Obj(i)       { List(i); i["id"] = ++Gold["id"] }
function has(i, k, f) { f=f?f:"Obj";i[k][0]; @f(i[k]); delete i[k][0]; return k}
function haS(i, k, f, x)    { i[k][0]; @f(i[k],x);   delete i[k][0] }
function hAS(i, k, f, x, y) { i[k][0]; @f(i[k],x,y); delete i[k][0] }

function push(x,a) { a[length(a)+1]=x; return x }

function o(a, prefix,     i,sep,s) {
  for(i in a) {s = s sep prefix a[i]; sep=","}
  return  s }

function rogues(   s,ignore) { 
  for(s in SYMTAB) 
    if (s ~ /^[_a-z]/) 
      print "#W> Rogue: "  s>"/dev/stderr" }

function oo(a,prefix,    indent,   i,txt) {
  txt = indent ? indent : (prefix ? prefix Gold["dot"] : "")
  if (!isarray(a)) {print(a); return a}
  ooSortOrder(a)
  for(i in a)  {
    if (isarray(a[i]))   {
      print(txt i"" )
      oo(a[i],"","|  " indent)
    } else
      print(txt i (a[i]==""?"": ": " a[i])) }}

function ooSortOrder(a, i) {
  for (i in a)
   return PROCINFO["sorted_in"] =\
     typeof(i+1)=="number" ? "@ind_num_asc" : "@ind_str_asc" }

function csv(a, f,       b4, g,txt,i,old,new) {
  f = f ? f : "-"             
  g = getline < f
  if (g< 0) { print "#E> Missing f ["f"]"; exit 1 } # file missing
  if (g==0) { close(f) ; return 0 }       # end of file                   
  txt = b4 $0                             # combine with prior
  gsub(/[ \t]+/,"",txt)
  if (txt ~ /,$/) { return csv(a,f,txt) } # continue txt into next
  sub(/#.*/, "", txt)                    # kill whitespace,comments    
  if (!txt)       { return csv(a,f,txt) } # skip blanks
  split(txt, a, ",")                      # split on "," into "a"
  for(i in a) {
     old = a[i]
     new = a[i]+0
     a[i] = (old == new) ? new : old
  }
  return 1 } 
