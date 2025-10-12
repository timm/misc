#!/usr/bin/env gawk -f
BEGIN   { BINS=5; FS = OFS = ","; SEED=1 }
NR==1   { srand(SEED) } 
        { gsub(/[ \t\r\n]/,"") 
          if (NR==1) {print head()} else {data()} }
END     { dump(Row); rogues() }

function dump(a,    r,c) {
  for(r in a)
    for(c in a[r]) bin(c, a[r][c]);
  for(r in a) {
    for(c in a[r])
      a[r][c] = bin(c, a[r][c]); 
    print(cat(a[r])) }}

function head(   i) { 
  for(i=1;i<=NF;i++) {
    if ($i ~ /-$/) Y[i]=0
    if ($i ~ /+$/) Y[i]=1
    if ($i ~ /^[A-Z]/ && $i !~ /X$/) {
      if (! (i in Y)) $i = tolower($i)
      Mu[i]=M2[i]=Sd[i]=N[i]=0 }}
  return $0 }

function data(     i,d,r) {
  r=rand()
  for(i=1;i<=NF;i++) {
    if ((i in Mu) && ($i != "?")) {
      $i += 0
      N[i]++
      d      = $i - Mu[i]
      Mu[i] += d / N[i]
      M2[i] += d * ($i - Mu[i])  
      Sd[i]  = N[i] < 2 ? 0 : (max(0,M2[i]) / (N[i]-1))^.5 }
    Row[r][i] = $i } }

function bin(i,v,    z,cdf,b,old,tmp) {
  if ((v != "?") && (i in Mu) && (! (i in Y))) {
    z   = (v - Mu[i]) / Sd[i]
    cdf = 1 / (1 + 2.718281^-z)
    b   =  min(BINS-1, max(0, int(BINS * cdf))) 
    old = Mins[i][b]  
    v = Mins[i][b] = min(old=="" ? 1E32 : old,v) }
  return v }

#--------------------------------------------------------------------
function rogues(     i) {
  for(i in SYMTAB)
    if(i !~ /^[_A-Z]/) print("Rogue? "i) }

function min(x,y) { return x<y ? x : y }  
function max(x,y) { return x>y ? x : y }

function cat(a,   s,i) {
  s = a[1]
  for(i=2; i<=length(a); i++) s = s "," a[i]
  return s }
