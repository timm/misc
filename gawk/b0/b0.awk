# vim: set expandtab tabstop=2 filetype=awk  :

BEGIN{ main() }

function add(i,x,    f) { if(x!="?") { i["n"]++; f="add"i["is"]; return @f(i,x) }}

function Data(i) {
  i["is"] = "Data"
  has(i,"rows")
  haS(i,"cols","Cols") }

function Cols(i) {
  i["is"] = "Cols"
  has(i,"all")
  has(i,"x")
  has(i,"y") }

function Col(i,at,txt) {
  i["n"]   = 0
  i["at"]  = at ? at : 0
  i["txt"] = txt ? txt : " " }
  
 function Num(i,at,txt) {
  Col(i,at,txt)
  i["is"] = "Num"
  i["n"] = i["mu"] = i["m2"] = i["sd"] = 0
  i["lo"] = -(i["hi"] = -1E32) 
  i["goal"] = (txt ~ /-$/ ? 0 : 1) }

function addNum(i,x,    d) {
  d = x - i["mu"]
  i["mu"] += d/i["n"]
  i["m2"] += d*(x-i["mu"])
  i["sd"]  = i["n"] < 2 ? 0 : (i["m2"]/(i["n"] - 1))^.5
  if (x > i["hi"]) i["hi"]=x
  if (x < i["lo"]) i["lo"]=x }

function Sym(i,at,txt) {
  Col(i,at,txt)
  i["is"] = "Sym"
  has(i,"count")
  i["most"] = i["mode"] = 0 }

function addSym(i,x,    tmp) {
  if ((tmp = ++i["count"][x]) > i["most"]) {
    i["most"] = tmp
    i["mode"] = x }}
  
function addData(i,a,     j,n) {
  n = length(i["cols"]["all"])
  for(j in a)
    n ? add(i["cols"]["all"][j], a[j]) : addCol(i, j, a[j]) }

function addCol(i,at,txt,    xy,kl) {
  xy = txt ~ /[!-+]$/ ? "y"   : "x" 
  kl = txt ~ /^[A-Z]/ ? "Num" : "Sym"
  i["cols"][xy] = at
  HAS(i["cols"]["all"], at, kl, at, txt) }

#------------------------------------------------------------------------------
function has(a,k)       { a[k][1];  delete a[k][1] }
function haS(a,k,f)     { has(a,k); return @f(a[k]) }
function hAS(a,k,f,x)   { has(a,k); return @f(a[k],x) }
function HAS(a,k,f,x,y) { has(a,k); return @f(a[k],x,y) }

function o(a,    j,pre) {
  if (typeof(a) != "array") return a
  pre = ("is" in a) ? a["is"] : ""
  for(j in a) return pre "{" (typeof(j)=="number" ? onums(a) : okeys(a)) "}" }

function onums(a,    s,j,sep) {
  s=sep=""; for(j in a) {s=s sep o(a[j]); sep=", "}; return s }

function okeys(a,    s,j,sep) {
  s=sep=""; for(j in a) if (j!="is") {s=s sep j"="o(a[j]); sep=", "}; return s }

function rogues(    j) {
  for(j in SYMTAB) if (j~/^[a-z_]/) print("? " j) }

function main(     j,f) {k=1
  for(j in ARGV) {
    f = "eg_" substr(ARGV[j],3)
    if (f in FUNCTAB) 
        @f(ARGV[j+1]) }
  rogues() }

#------------------------------------------------------------------------------
function eg_one(_) { print 1}
function eg_data(_,  j) { Data(j); print o(j)} 
function eg_num(_,  j) { Num(j); print o(j)} 
function eg_sym(_,  j) { Sym(j); print o(j)} 



