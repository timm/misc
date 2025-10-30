#!/usr/bin/env gawk -f
BEGIN { FS=" "; header() }
      { confuse($1,$2) }
END   { confused(); rogues() }

function header() {
  printf "%5s %5s %5s %5s %5s %5s %5s %5s %20s\n",
    "tn","fn","fp","tp","pd","prec","pf","acc","label" }

function confuse(want,got,    c,tmp) {
  split(want " " got, tmp)
  for(c in tmp)
    if(!(tmp[c] in cf)) {
      cf[tmp[c]]["tn"] = total
      cf[tmp[c]]["fn"] = cf[tmp[c]]["fp"] = cf[tmp[c]]["tp"] = 0 }
  for(c in cf) {
    if(c==want) { cf[c]["tp"]+=(got==want); cf[c]["fn"]+=(got!=want) }
    else { cf[c]["fp"]+=(got==c); cf[c]["tn"]+=(got!=c) }}
  total++ }

function confused(    c,tn,fn,fp,tp) {
  for(c in cf) {
    tn+=cf[c]["tn"]; fn+=cf[c]["fn"]; fp+=cf[c]["fp"]; tp+=cf[c]["tp"]
    report(c,cf[c]["tn"],cf[c]["fn"],cf[c]["fp"],cf[c]["tp"]) }
  report("_OVERALL",tn,fn,fp,tp) }

function report(lbl,tn,fn,fp,tp,    pd,prec,pf,acc,d) {
  d = tp+fn+fp+tn
  pd=100*tp/(tp+fn+1E-32); prec=100*tp/(fp+tp+1E-32)
  pf=100*fp/(fp+tn+1E-32); acc=100*(tp+tn)/(d+1E-32)
  printf "%5.0f %5.0f %5.0f %5.0f %5.0f %5.0f %5.0f %5.0f %20s\n",
    tn,fn,fp,tp,pd,prec,pf,acc,lbl }

function rogues(    i,s) {
  for(i in SYMTAB) if (i ~ /^[a-z]/) s= s " " i
  if (s) print "?confuse " s > "/dev/stderr" }
 
