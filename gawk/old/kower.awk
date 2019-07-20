#/usr/bin/env gawk -f
# vim: ts=2 sw=2 sts=2 et:
#-------- -------- -------- -------- -------- --------

BEGIN { 
  FS="," 
  WAIT=100
  INF=10^32
  NUM="-?[0-9]+(\.[0-9]+)?"
  new(CACHE)
  B=3
  at(Bins,2,"                  -0.43,    0.43")
  at(Bins,3,"                  -0.67, 0, 0.67")
  at(Bins,4,"            -0.84,-0.25,    0.25, 0.84")
  at(Bins,5,"            -0.97,-0.43, 0, 0.43, 0.97")
  at(Bins,6,"      -1.07,-0.57,-0.18,    0.18, 0.57, 1.07")
  at(Bins,7,"      -1.15,-0.67,-0.32, 0, 0.32, 0.67, 1.15")
  at(Bins,8," 1.22,-0.76,-0.43,-0.14,    0.14, 0.43, 0.76, 1.22")
  at(Bins,9,"-1.28,-0.84,-0.52,-0.25, 0, 0.25, 0.52, 0.84, 1.28")
}
function at(a,i,str,   b,j,k,l) {
  split(str,b,",")
  for(j in b) {
    k = b[j]
    l = k+0
    a[i][j] = k==l ? l : k } 
}
function setup(c,v) {
  if (v~NUM) {
    Sd[c] = Mu[c] = M2[c] = N[c] = 0
    Hi[c] = -1*INF
    Lo[c] = INF
  } else {
    new(Cnt); new(Mode); new(Most); Enc[c]=0
}}
function bin(c,v,  n,m,i) {
  if (v=="?") return v
  if (!(c in Mu)) return v
  n = (Mu[c] - v) / Sd[v]
  m = length(Bins[B])
  for(i=1;i<= m;i++) 
    if (n < Bins[B][i]) return Mu[c] + Bins[B][i]*Sd[c]
  return m
}
function add1(c,v) {
  if (c != "?") {
    if (N[c] == 0) 
      setup(c,v)
    v= (c in Mu[c]) ? num1(c,v) : sym1(c,v) 
  return v
}
function num1(c,v,   d) {
  N[c]++
  d      = x - Mu[c] 
  Mu[c] += d/N[c]
  M2[c] += d*(x - Mu[c])
  Sd[c] =  M2[c] / ( N[c] - 1 + 1/INF )^0.5  
  if (v > Hi[c]) Hi[c] = v 
  if (v < Lo[c]) Lo[c] = v
}
function sym1(c,v) {
  N[c]++
  Ent[c] = 0
  if (++Cnt[c][v] > Most[c]) {
    Most[c] = Cnt[c][v]
    Mode[c] = v }
}  
function ent(c,  v,p,e) {
  if (! Ent[c]) {
    for (v in Cnt[c]) {
      p  = Cnt[c][v] / N[c]
      e -= p*log(p)/log(2) }
    Ent[c] = e }
  return Ent[c]
}
function eat(row,r) {
  for(c in row[r]) {
    old = data[r][c]
    data[r][c] = bin(c, row[r][c]
}
     { gsub(/[ \t\r]*/,""); gsub(/#.*$/,"") }
/^$/ { next }
     { at(CACHE,++R,$0)
       for(C in CACHE[R]) 
         add1(C, CACHE[R][C])
       if (! (R % WAIT)) {
         for(R in CACHE) eat(CACHE,R)
         new(CACHE) }}
