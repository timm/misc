#!/usr/bin/env gawk -f
BEGIN {D=-1
       FILE="../../data/auto93.csv"
       SEED=1234567891
       go(ARGV[1])
}
#------------------------------------------------------------------------------------------
function make(d) {
  array(SEEN,d); array(ROW,d); array(NS,d); array(MU,d);  array(M2,d); array(HI,d); array(LO,d) }

function kill(d) {
  delete SEEN[d];delete ROW[d];delete NS[d];delete MU[d]; delete M2[d];delete HI[d];delete LO[d] }

function array(x,i) { x[i][1]; delete x[i][1] }

function new(d,lst,    c) {
  make(d)
  if (d==0) 
   for(c in lst) {
     NAME[c] = lst[c] ; 
     NAME[c] ~ /[-!+]$/ ? YS[c] : XS[c] }
  for(c in NAME)  
     if (NAME[c] !~ /X$/) 
       NAME[c] ~ /^[A-Z]/ ? makeNum(d,c,NAME[c]) : makeSym(d,c) }

function makeNum(d,c,name) {
  if (d==0) HEAVEN[c] = name ~ /-$/ ? 0 :1
  MU[d][c] = M2[d][c] = NS[d][c] = 0
  LO[d][c] = 1E30; HI[d][c] = -1e30 }

function makeSym(d,c) {
  NS[d][c]=0
  has(SEEN[d],c) }
#------------------------------------------------------------------------------------------
function isNum(c) { return c in HEAVEN }

function adds(d,lsts,      r) { for(r in lsts) add(d,lsts[r]) }

function add(d,lst,    r,c) {
  r=randint(10^6)
  for(c in NAME) add1(d,r,c,lst[c]) }

function add1(d,r,c,x) {
  ROW[d][r][c] = x
  if (x != "?") {
    NS[d][c]++ 
    isNum(c) ? add2Num(d,r,c,x) : add2Sym(d,r,c,x)} }

function add2Num(d,r,c,x,    delta) {
  delta     = x - MU[d][c]
  MU[d][c] += delta/NS[d][c]
  M2[d][c] += delta *(x- MU[d][c])
  if (x< LO[d][c]) LO[d][c] = x
  if (x> HI[d][c]) HI[d][c] = x }

function add2Sym(d,r,c,x) { SEEN[d][r][c][x]++}
#------------------------------------------------------------------------------------------
function mid(d,c) {
  return isNum(c) ? MU[d][c] : mode(SEEN[d][c]) }
         
function div(d,c) {
  return isNum(c) ? sd(d,c) : entropy(SEEN[d][c]) }

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
function has(a,i) { a[i][1]; delete a[i][1] }

function randint(n) { return int(n * rand()) }

function rogues(    x) { for(x in SYMTAB) if (x~/^[a-z]/) print "W> rogue? "x }

function coerce(s,    t) { t=s+0; return t==s? t : s }

function csv(f,a) {
  split("",a,"")
  f = f?f:"-"
  if(getline < f) {
    gsub(/[ \t\r]/,"",$0)
    split($0,a,/,/) 
    return length(a) > 0
  } else 
    close(f) }

function csv2data(f,   row,first) {
  first=1
  while(csv(f,row)) {
    first ? new(++D,row) : add(D,row)
    first = 0 }
  return D }

function go(x,   fun) {
  fun = "go_" x
  if (fun in FUNCTAB) { srand(SEED); @fun()}
  rogues() }
#------------------------------------------------------------------------------------------
function go_all(   f,a) {
  for(f in FUNCTAB) {
   split(f,a,/_/)
   if (a[1]=="go" && a[2] !="all") go(a[2]) }}

function go_save() { system("git commit -am saved; git push; git status") }

function go_csv(   d,c,r) { 
  d = csv2data(FILE)
  for(d in MU)
  for(c in MU[d]) print(NAME[c],MU[d][c],sd(d,c), HEAVEN[c])
  print(length(ROW[d]))}

function go_cat(  f) {
  while(getline < (f?f:"-")) print $0 }
#------------------------------------------------------------------------------------------
