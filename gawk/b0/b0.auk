# vim: set expandtab tabstop=2 filetype=awk  :

BEGIN{ main() }

function add(i,x,    f) { f="add"i.is; return @f(i,x) }

function Data(i) {
  i.is = "Data"
  have(i,"rows")
  havE(i,"cols","Cols") }

function Cols(i) { #adasasdas
  i.is = "Cols"
  have(i,"names")
  have(i,"all")
  have(i,"x")
  have(i,"y") }

function Col(i,txt,at) {
  i.n   = 0
  i.at  = at ? at : 0
  i.txt = txt ? txt : " " }
  
function Num(i,txt,at) {
  Col(i,at,txt)
  i.is = "Num"
  i.n = i.mu = i.m2 = i.sd = 0
  i.lo = -(i.hi = -1E32) 
  i.goal = (txt ~ /-$/ ? 0 : 1) }

function Sym(i,txt,at) {
  Col(i,txt,at)
  i.is = "Sym"
  have(i,"count")
  i.most = i.mode = 0 }

#------------------------------------------------------------------------------
# asdas
function addNum(i,x,    d) {
  if (x=="?") return
  i.n++
  d     = x - i.mu
  i.mu += d/i.n
  i.m2 += d*(x-i.mu)
  i.sd  = i.n < 2 ? 0 : (i.m2/(i.n - 1))^.5
  if (x > i.hi) i.hi=x
  if (x < i.lo) i.lo=x }

function addSym(i,x,    tmp) {
  if (x=="?") return
  i.n++
  if ((tmp = ++i.count[x]) > i.most) {
    i.most = tmp
    i.mode = x }}
  
function addData(i,a,     j,n) {
  n = length(i.cols.all)
  for(j in a)
    n ? add(i.cols.all[j], a[j]) : addCol(i, j, a[j]) }

function addCol(i,at,txt,    xy,type) {
  i.cols.names[at] = txt
  xy   = txt ~ /[!-+]$/ ? "y"   : "x" 
  type = txt ~ /^[A-Z]/ ? "Num" : "Sym"
  i.cols[xy] = at
  hAVE(i.cols.all, at, type, at, txt) }

#------------------------------------------------------------------------------
function have(a,k)         { a[k][1];  delete a[k][1] }
function havE(a,k,f)       { have(a,k); return @f(a[k]) }
function haVE(a,k,f,x)     { have(a,k); return @f(a[k],x) }
function hAVE(a,k,f,x,y)   { have(a,k); return @f(a[k],x,y) }
function HAVE(a,k,f,x,y,z) { have(a,k); return @f(a[k],x,y,z) }

function o(a,    j,pre) {
  if (typeof(a) != "array") return a
  pre = ("is" in a) ? a.is : ""
  for(j in a) return pre "{" (typeof(j)=="number" ? onums(a) : okeys(a)) "}" }

function onums(a,    s,j,sep) {
  s=sep=""; for(j in a) {s=s sep o(a[j]); sep=", "}; return s }

function okeys(a,    s,j,sep) {
  s=sep=""; for(j in a) if (j!="is") {s=s sep j"="o(a[j]); sep=", "}; return s }

function rogues(    j) {
  for(j in SYMTAB) if (j~/^[a-z_]/) print("? " j) }

function main(     j,f) {
  for(j in ARGV) {
    f = "eg_" substr(ARGV[j],3)
    if (f in FUNCTAB) 
        @f(ARGV[j+1]) }
  rogues() }

#------------------------------------------------------------------------------
function eg_one(_) { print 1}
function eg_data(_,  j) { Data(j); print o(j)} 
function eg_num(_,  j) { Num(j,"num"); print o(j)} 
function eg_sym(_,  j) { Sym(j); print o(j)} 
