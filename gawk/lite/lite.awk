BEGIN {D=0} 
#------------------------------------------------------------------------------------------
function malloc(d) {
  array(seen,d); array(row,d); array(ns,d); array(mu,d);  array(m2,d); array(hi,d); array(lo,d) }

function free(d) {
  delete seen[d];delete row[d];delete ns[d];delete mu[d]; delete m2[d];delete hi[d];delete lo[d] }

function array(x,i) { x[i][1]; delete x[i][1] }

function new(d,header,    c) {
  malloc(d)
  if (d==0) split(header,NAME,",")
  for(c in NAME) { 
     ALL[c]
     if (NAME[c] ~ /X$/) {
       NAME[c] ~ /[-!+]$/ ? YS[c] : XS[c]
       if (NAME[c] ~ /^[A-Z]/)? makeNum(d,c,NAME[c]) : makeSym(d,c) }}}

function makeNum(d,c) {
  HEAVEN[c] = NAME[c] ~ /-$/ ? -1 :1
  mu[d][c] = m2[d][c] = ns[d][c] = 0
  lo[d][c] = 1E30; hi[d][c] = -1e30 }

function nameSyn(d,c) {
  ns[d][c]=0
  has(seen[d],c) }
  
function isNum(c) { return c in mu[d] }
#------------------------------------------------------------------------------------------
function adds(d,lsts,      r) { for(r in lsts) add(d,lsts[r]) }

function add(d,lst,    r) {
  r=randint(10^6)
  for(c in ALL) add1(d,r,c,lst[c]) }

function add1(d,r,c,x) {
  row[d][r][c] = x
  if (x != "?") {
    ns[d][c]++ 
    isNum(c) ? add2Num(d,r,c,x) : add2Sym(d,r,c,x)} }

function add2Num(d,r,c,x) {
  delta     = x - mu[d][c]
  mu[d][c] += delta/ns[d][c]
  m2[d][c] += delta *(x- mu[d][c])
  if (x< lo[d][c]) lo[d][c] = x
  if (x> hi[d][c]) hi[d][c] = x }

function add2Sym(d,r,c,x) { seen[d][r][c][x]++}
#------------------------------------------------------------------------------------------
function mid(d,c) {
  return isNum(c) ? mu[d][c] : mode(seen[d][c]) }
         
function div(d,c) {
  return isNum(c) ? sd(n,c) : entropy(seen[d][c])       

function sd(d,c) {
  return ns[d][c] < 2 ? 0 : (m2[d][c]/(ns[d][c] - 1))**.5 }

function mode(lst,    n,out) {
  for (x in lst) if (lst[x] > n) { n=lst[i]; out=x }
  return out

function entropy(lst,   n,e) {
  for(c in lst) n += lst[c]
  for(c in lst) e -= lst[c]/n * log(lst[c]/n)/ log(2)
  return e }
#------------------------------------------------------------------------------------------
function randint(n) { return int(n * rand()) }
