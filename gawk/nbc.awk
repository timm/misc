#!/usr/bin/env gawk -f
BEGIN { FS=","; WAIT=20; M=2; K=1 }
      { gsub(/[ \t\r]/,"") }
NR==1 { head() }
NR>1  { if (NR > WAIT+1) print $klass, predict()
        train($klass) }
END   { rogues() }

function head(    i) {
  for(i=1;i<=NF;i++)
    if ($i ~ /[!]$/) klass = i }

function train(actual,    i) {
  nk[actual]++
  for(i=1;i<=NF;i++)
    if (i != klass)
      f[i][$i][actual]++ }

function predict(    i,k,like,best,max) {
  max = -1E32
  for(k in nk) {
    like = log((nk[k] + K) / (NR-1 + K*length(nk)))
    for(i=1;i<=NF;i++) {
      if (i != klass) {
        like += log((f[i][$i][k] + M) / (nk[k] + M*length(f[i]))) }}
    if (like > max) {
      max = like
      best = k }}
  return best }

function rogues(    i,s) {
  for(i in SYMTAB) if (i ~ /^[a-z]/) s= s " " i
  if (s) print "?nbc " s > "/dev/stderr" }
