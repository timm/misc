#!/usr/bin/env gawk -f
BEGIN   { BINS=7; FS = OFS = "," }
        { gsub(/[ \t\r\n]/,"") }
NR==1   { print; head(); next }
        { data() }

function head( i) { 
  for(i=1;i<=NF;i++) {
    if ($i ~ /-$/) Y[i]=0
    if ($i ~ /+$/) Y[i]=1
    if ($i ~ /^[A-Z]/ && $i !~ /X$/)
      Mu[i]=M2[i]=Sd[i]=N[i]=0 }}

function data(     i,d) {
  for(i=1;i<=NF;i++) {
    if ((i in Mu) && ($i != "?")) {
      $i += 0
      N[i]++
      d      = $i - Mu[i]
      Mu[i] += d / N[i]
      M2[i] += d * ($i - Mu[i])  
      Sd[i]  = N[i] < 2 ? 0 : (max(0,M2[i]) / (N[i]-1))^.5 }
    Row[NR-1][i] = $i } }

function mins(      i,v,b,r) {
  for(r in Row) 
    for(i in Mu) 
      if((v=Row[r][i]) != "?") {
        b = bucket(i, v)
        if (_min[i][b] == "" || v < _min[i][b])
          _min[i][b] = v } }

function bucket(i,v,    z,cdf) {
  z   = (v - Mu[i]) / Sd[i]
  cdf = 1 / (1 + 2.718281^-z)
  return min(BINS-1, max(0, int(BINS * cdf))) }

function bin(i,v,    b) {
  if ((v != "?") && (i in Mu) && (! (i in Y))) {
    b =  bucket(i, v) 
    return (_min[i][b] != "") ? _min[i][b] : v }
  return v }

function dump(    r,i) {
  for(r in Row) {
    print("\n" cat(Row[r]))
    for(i in Mu) 
      Row[r][i] = bin(i, Row[r][i])
    print(cat(Row[r])) }}

function rogues(     i) {
  for(i in SYMTAB)
    if(i !~ /^[_A-Z]/) print("Rogue? "i) }

#--------------------------------------------------------------------
function min(x,y) { return x<y ? x : y }  
function max(x,y) { return x>y ? x : y }

function cat(a,   s,i) {
  s = a[1]
  for(i=2; i<=length(a); i++) s = s "," a[i]
  return s }

