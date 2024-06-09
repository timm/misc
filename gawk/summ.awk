BEGIN {FS=","}
# NR==1 { head(Cols) }
#       { body(Cols,Rows) }
#
#----------------------------------------------------------------------------------------
# Polymorphic verbs
function add(i,x,    f) { f="add" i[isA]; return @f(i,x) }
function mid(i,      f) { f="mid" i[isA]; return @f(i)   }
function div(i,      f) { f="div" i[isA]; return @f(i)   }

#----------------------------------------------------------------------------------------
function Col(i,at,txt,isa) {
  i[isA="isa"]=isa
  i[aT="at"]=at; i[txT="txt"]=txt; i[N="n"]=0 }

#----------------------------------------------------------------------------------------
function Num(i,at,txt) { 
   Col(i,at,txt,"Num")
   i[mU="mU"] = i[m2="m2"] = i[sD="sD"] = 0
   i[lO="lO"] =  1e30
   i[hI="hI"] = -1e30 }

function divNum(i) { return i[mU] }
function midNum(i) { return i[sD] }

function addNum(i,x,     d) {
  if (x != "?") {
    i[N]++
    if (x > i[hI]) i[hI]=x
    if (x < i[lO]) i[lO]=x  
    d = x - i[mU]
    i[mU] += d/i[N]
    i[m2] += d*(x - i[mU])
    i[sD]  = (i[m2] / (i[N] - 1 + 1E-30))^0.5 }}

#----------------------------------------------------------------------------------------
function Sym(i,at,txt) { 
   Col(i,at,txt,"Sym")
   array(i, haS="has")
   i[modE="mode"]=""
   i[mosT="most"]=0 }

function divSym(i) { return i[modE] }
function midSym(i) { return entropy(i[haS]) }

function addSym(i,x) {
  if (x != "?") {
    i[N]++
    if (++i[seeN][x] > i[mosT]) {
      i[mosT] = i[seeN][x]
      i[modE] = x }}}

function head(cols,    k,what) {
  for(k=1;k<=NF;k++) {
    array(cols,k)
    what = $k ~ /^[A-Z]/ ? "Num" : "Sym" 
    @what(cols[k], k,$k) }}

function body(cols,    k) { for(k in Cols) add(cols[k],$k) }

function array(a,k) { a[k][0]; delete a[k][0] }

function entropy(a,   N,e) {
  for(k in a) N += a[k]
  for(k in a) e -= a[k]/N * log( a[k] / N ) / log(2)
  return e }
