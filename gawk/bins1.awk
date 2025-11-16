#!/usr/bin/env gawk -f
# (c) 2025, Tim Menzies, MIT license
BEGIN { OFS=FS=","; BINS=5}
      { gsub(/[ \t]+/,"") }
NR==1 { for(c=1;c<=NF;c++) head(c,$c) }
NR>1  { for(c in Num) if (Num[c] && $c != "?") $c = welford(c,$c);
        count(disty()) }
END   { report(); rogues() }

function head(c,v) {
  Name[c] = v;
  Num[c] = v ~ /^[A-Z]/;
  if      (v ~ /+$/)  Y[c] = 1;
  else if (v ~ /-$/)  Y[c] = 0; }

function welford(c,v,     d) {
  v += 0;
  N[c]++;
  d      = v - Mu[c];
  Mu[c] += d / N[c];
  M2[c] += d*(v - Mu[c]);
  Sd[c]  = N[c] < 2 ? 0 : sqrt(M2[c] / (N[c] - 1));
  return v }

function disty(     d,n,c) {
  for(c in Y) { n++; d += (norm(c,$c) - Y[c])^2 }
  return sqrt(d/n) }

function norm(c,v) {
  return 1/(1 + exp(-1.7*(v - Mu[c]) / (Sd[c] + 1e-32))) }

function bin(c,v) {
  if (v == "?") return v;
  if (!Num[c]) return v;
  return int(BINS * norm(c,v)) }

function count(d,    dn,c,v,what) {
  d  = welford(NF+1, d);
  dn = norm(NF+1, d);
  what = dn <= sqrt(NR-1)/(NR-1);
  if (what) Best++;
  for(c in Name)
    if (!(c in Y)) {
      v = bin(c,$c);
      Freq[c][v][what]++ }}

function report(    b,r,c,v,a,i,z) {
  for(c in Freq)
    for(v in Freq[c]) {
      b = Freq[c][v][1] / Best;
      r = Freq[c][v][0] / (NR - 1 - Best);
      a[++i]["="] = b - r;
      a[  i]["col"] = c;
      a[  i]["val"] = v }
  asort(a, z, "keyeq");
  for(i=1; i<=length(z); i++)
    print i, z[i]["="], Name[z[i]["col"]], z[i]["val"] }

function compare(a, b) { return a < b ? -1 : (a > b ? 1 : 0) }

function keyeq(i1, a1, i2, a2) { return compare(a2["="], a1["="]) }   

function rogues(    i,p) {
  p="^(NF|NR|FS|RS|RT|FNR|OFS|ORS|PREC|ARGC|ARGV|OFMT|LINT|FPAT|" \
    "ERRNO|RSTART|ARGIND|SUBSEP|CONVFMT|ENVIRON|SYMTAB|FUNCTAB|PROCINFO|" \
    "FILENAME|RLENGTH|BINMODE|IGNORECASE|FIELDWIDTHS|ROUNDMODE|TEXTDOMAIN)$"
  for(i in SYMTAB) if (i !~ p) printf " ?" i  > "/dev/stderr" }
