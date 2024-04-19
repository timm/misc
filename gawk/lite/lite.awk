#!/usr/bin/env gawk -f
BEGIN {D=0
       SEED=1234567891
       go(ARGV[1])
}
#------------------------------------------------------------------------------------------
function malloc(d) {
  array(SEEN,d); array(ROW,d); array(NS,d); array(MU,d);  array(M2,d); array(HI,d); array(LO,d) }

function free(d) {
  delete SEEN[d];delete ROW[d];delete NS[d];delete MU[d]; delete M2[d];delete HI[d];delete LO[d] }

function array(x,i) { x[i][1]; delete x[i][1] }

function new(d,header,    c) {
  malloc(d)
  if (d==0) split(header,NAME,",")
  for(c in NAME) { 
     if (d==0) ALL[c]
     if (NAME[c] !~ /X$/) {
       if (d==0) NAME[c] ~ /[-!+]$/ ? YS[c] : XS[c]
       NAME[c] ~ /^[A-Z]/ ? makeNum(d,c,NAME[c]) : makeSym(d,c) }}}

function makeNum(d,c,name) {
  if (d==0) HEAVEN[c] = name ~ /-$/ ? -1 :1
  MU[d][c] = M2[d][c] = NS[d][c] = 0
  LO[d][c] = 1E30; HI[d][c] = -1e30 }

function nameSyn(d,c) {
  NS[d][c]=0
  has(SEEN[d],c) }
  
#------------------------------------------------------------------------------------------
function isNum(d,c) { return c in MU[d] }

function adds(d,lsts,      r) { for(r in lsts) add(d,lsts[r]) }

function add(d,lst,    r,c) {
  r=randint(10^6)
  for(c in ALL) add1(d,r,c,lst[c]) }

function add1(d,r,c,x) {
  ROW[d][r][c] = x
  if (x != "?") {
    NS[d][c]++ 
    isNum(d,c) ? add2Num(d,r,c,x) : add2Sym(d,r,c,x)} }

function add2Num(d,r,c,x,    delta) {
  delta     = x - MU[d][c]
  MU[d][c] += delta/NS[d][c]
  M2[d][c] += delta *(x- MU[d][c])
  if (x< LO[d][c]) LO[d][c] = x
  if (x> HI[d][c]) HI[d][c] = x }

function add2Sym(d,r,c,x) { SEEN[d][r][c][x]++}
#------------------------------------------------------------------------------------------
function mid(d,c) {
  return isNum(d,c) ? MU[d][c] : mode(SEEN[d][c]) }
         
function div(d,c) {
  return isNum(d,c) ? sd(d,c) : entropy(SEEN[d][c]) }

function sd(d,c) {
  return NS[d][c] < 2 ? 0 : (M2[d][c]/(NS[d][c] - 1))**.5 }

function mode(lst,    n,out,x) {
  for (x in lst) if (lst[x] > n) { n=lst[x]; out=x }
  return out }

function entropy(lst,   n,e,c) {
  for(c in lst) n += lst[c]
  for(c in lst) e += lst[c] / n * log(lst[c]/n) / log(2)
  return -e }
#------------------------------------------------------------------------------------------
function randint(n) { return int(n * rand()) }

function rogues(    x) { for(x in SYMTAB) if (x~/^[a-z]/) print "W> rogue? "x }

function go(x,   fun) {
  fun = "go_" x
  if (fun in FUNCTAB) { srand(SEED); @fun()}
  rogues() }
#------------------------------------------------------------------------------------------
function go_all(   f,a) {
  for(f in FUNCTAB) {
   split(f,a,/_/)
   if (a[1]=="go" && a[2] !="all") go(a[2]) }}

function go_cat(  f) {
  while(getline < (f?f:"-")) print $0 }
#------------------------------------------------------------------------------------------
