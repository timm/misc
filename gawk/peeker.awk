# vim: ft=awk ts=2 sw=2 et :

BEGIN                   { List(GOLD) }
function List(i)        { split("",i,"") }
function Object(i)      { List(i); i["id"] = ++GOLD["id"] }
function has(i,k,f)     { f=f?f"List";i[k][0]; @f(i[k]);     delete i[k][0] }
function haS(i,k,f,x)   {             i[k][0]; @f(i[k],x);   delete i[k][0] }
function hAS(i,k,f,x,y) {             i[k][0]; @f(i[k],x,y); delete i[k][0] }
function have(i,f,k)    { k=length(k)+1; has(i,k,f); return k }
G
function add(i,x,  f) { f=i["is"] "Add"; return @f(i,x) }

function tabs(   a) {
  tab(a)
  oo(a)
}
function tab(i) {
  Object(i); i["is"] = "tab"
  has(i,"nums"); has(i,"cols"); has(i,"xs"); has(i,"ys");
}
function tabAdd(i,a) {
  length(i["cols"]) ?  tabData(i,a) : tabHeader(i,a)
}
function tabHeader(i,a,   where, what, j) {
  for(j=1; j<=length(a); j++) {
    what = a[j] ~ /[:<>]/ ?  "num" : "sym"
    what = a[j] ~ /\?/    ? "none" : what
    where= a[j] ~ /[!<>]/ ?  "ys"  : "xs"
    has(i["cols"], j, what)   
    i[where][j]
}}
function tabHeder(i,a, b,   r,j) {
  r = have(i["rows"], "row")
  for(j=1; j<=length(a); j++) 
    i["rows"]["cells"][j] = add(i["cols"][j], a[j])
}
function row(i) {
  Object(i)
  has(i,"cells")
  has(i,"ranges")
}
function col(i,s,n) { 
  List(i); i["is"]="col"
  i["txt"]=s; i["pos"]=n; 
}
function none(i,s,n)  { col(i,s,n); i["is"]="sym" }
function noneAdd(i,x) { return x }

function sym(i,s,n) { 
  col(i,s,n); i["is"]="sym"
  i["mode"]= i["most"]= "" 
}
function symAdd(i,x,   n) {
  if(x=="?") return x
  i["n"]++
  n= ++i["seen"][x]
  if (n> i["most"]) { i["mode"]=x; i["most"]=n}
  return x
}  
function num(i,s,n) { 
  col(i,s,n); i["is"]="num"
  i["hi"] = -1E32
  i["lo"] =  1E32
  i["mu"]= i["m2"]= i["n"]= i["sd"]=0
}
function numAdd(i,x,   d) {
  if(x=="?") return x
  i["n"]++
  if(x > i["hi"]) i["hi"] = x
  if(x < i["lo"]) i["lo"] = x
  d        = x - i["mu"]
  i["mu"] += d / i["n"]
  i["m2"] += d * (x - i["mu"]) 
  if (i["n"]  < 2) i["sd"] = 00  
  else i["sd"] = (i["m2"] <= 0)?0:(i["m2"]/(i["n"]-1))^0.5 
  return x
}

#### lib
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

BEGIN {tabs() }

function rogues(s) { f
  for(s in SYMTAB) if (s ~ /^[_a-z]/) print "#W> Rogue: " s>"/dev/stderr" 
}
BEGIN { rogues() }


