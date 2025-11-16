#!/usr/bin/env gawk -f
# (c) 2025, Tim Menzies, MIT license
src(){ cat <<'EOF'
BEGIN{OFS=FS=","}
     {gsub(/[ \t]+/,"")}

NR==1{header()}
NR>1{row()}
END{report()}

function header(   c){ for(c=1;c<=NF;c++) colAdd(c,$c) }

function colAdd(c,txt,    n){
  n=Data.cols.num[c]
  n.txt=txt; n.n=0; n.mu=0; n.m2=0; n.sd=0
  if(txt~/^_/){Data.ys[c]=1; n.best=0}else Data.xs[c]=1 }

function row(   c,x,isB,t,dn){
  Data.N++
  for(c in Data.cols.num){x=$c; add(Data.cols.num[c],x)}
  dn = disty()
  t = sqrt(Data.N-1)/(Data.N-1)
  isB = (dn <= t); if(isB) Data.B++
  for(c in Data.xs){x=$c; Freq.all[c][x]++; if(isB) Freq.B[c][x]++} }

function add(n,x,    d){
  n.n++; d=x-n.mu; n.mu+=d/n.n; n.m2+=d*(x-n.mu)
  if(n.n>1) n.sd=sqrt(n.m2/(n.n-1)) }

function norm(c,x,    lo,hi,mu,sd){
  sd=Data.cols.num[c].sd
  if(sd==0) return 0.5
  return (x-Data.cols.num[c].mu)/(sd) }

function disty(   d,n,c,delta){
  d=0; n=0
  for(c in Data.ys){
    delta = norm(c,$c) - Data.cols.num[c].best
    d += delta*delta
    n++ }
  return sqrt(d/n) }

function report(){ print "N",Data.N,"B",Data.B }
EOF
}

prep(){ gawk '{gsub(/\.([A-Za-z_][A-Za-z0-9_]*)/,"[\"\\1\"]");print}'; }
gawk -f <(src | prep) "$@"
