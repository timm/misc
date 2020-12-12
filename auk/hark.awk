@include "lib"

function add(i,x,  f) { oo(i); f= i.is"Add"; return @f(i,x) }

#---------------------------------
function Col(i,pos,txt) {
  Obj(i)
  i.is="Col"
  i.pos=pos
  i.txt=txt
  i.n  =0
  i.w  =txt ~ /</ ? -1 : 1 }

#---------------------------------
function Skip(i,pos,txt) { Col(i,pos,txt); i.is = "Skip" }
function _Add(i,x)       { return x }

#---------------------------------
function Sym(i,pos,txt) {
  Col(i, pos,txt)
  i.is = "Sym"
  has(i,"seen")
  i.mode=i.most="" }

function _Add(i,x,    d,n) {
  if (x!="?") {
    n = ++i.some[x]
   if(n>i.most) { i.most=n; i.mode=x} }
  return x }

#---------------------------------
function Some(i,pos,txt) {
  Col(i,pos,txt)
  i.is="Some"
  i.ok= 1
  i.max=128
  i.lo =  1E30
  i.hi = -1E30
  has(i,"all") }

function _Add(i,x,    len,pos) {
  if (x != "?") {
    i.n++
    len=length(i.all)
    if (i.n <= i.max) pos=len + 1
    else  {if (rand() < i.max/i.n) pos=int(len*rand())}
    if (pos) {
      if (x < i.lo) i.lo = x
      if (x > i.hi) i.hi = x
      i.ok=0
      i.all[pos]=x }}
  return x }

function _Ok(i) { if (!i.ok) i.ok=asort(i.all) }

function _Norm(i,x) {
  if (x=="?") return x
  x= (x-i.lo) / (i.hi - i.lo +1E-32)
  return x<0 ? 0 : (x>1 ? 1 : x) }

#---------------------------------
function Row(i,a,t,     j) {
  Obj(i)
  has(i,"cells") 
  for(j in a) i.cells[j] = add(t.cols[j], a[j]) }

function _Gt(i,j,t,   
                 n,e,c,w,x,y,sum1,sum2) {
  n = length(t.ys)
  for(c in t.ys) {
    w     = t.cols[c].w
    x     = SomeNorm(t.cols[c], i.cells[c])
    y     = SomeNorm(t.cols[c], j.cells[c])
    sum1 -= 2.71828 ^ ( w * (x - y)/n )
    sum2 -= 2.71828 ^ ( w * (y - x)/n )
  }
 return sum1/n < sum2/n }

#---------------------------------
function Tab(i) {
  Obj(i)
  i.is = "Tab"
  has(i,"xs")
  has(i,"ys")
  has(i,"rows")
  has(i,"cols") }

function _What(i,pos,txt,  x,where) {
  x="Sym"
  if (txt ~ /[<>:]/) x="Some"
  if (txt ~ /\?/)    x="Skip"
  if (x != "Skip") {
    where =  txt ~ /[<>!]/ ? "ys" : "xs"
    i[where][pos] }
  return x }
 
function _Add(i,a,    j) {
  if (length(i.cols)) 
    hAS(i.rows, int(1E9 * rand()) ,"Row",a,i)
  else
    for(j in a)
      hAS(i.cols, j, _What(i,j,a[j]), j, a[j]) }

function _Doms(i,   n,j,k) {
  n= 100
  for(j in i.rows)
    for(k in i.rows)
      if(i.rows[j].id > i.rows[k].id) {
        if (--n<0) return
        i.rows[j].dom += RowGt(i.rows[j], i.rows[k]) }}

function _Read(i,f,  a) {  while(csv(a,f)) add(i,a) }

#---------------------------------
function demo1(f,    i) {
  Tab(i)
  TabRead(i,f ? f : "weather" Gold.dot "csv") 
  #TabDoms(i)
#  oo(i)
}

BEGIN {  demo1(); rogues() }
