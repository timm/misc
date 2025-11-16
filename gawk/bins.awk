#!/usr/bin/env gawk -f
# (c) 2025, Tim Menzies, MIT license

BEGIN { OFS=FS=","; BINS=5}
      { gsub(/[ \t]+/,"") }
NR==1 { for(c=1;c<=NF;c++) head(c,$c) }
NR>1  { for(c in Num) if ($c != "?") $c = welford(c,$c);
        count(disty()) }
END   { report(); rogues() }

function head(c,v) {
  Names[c]=v;
  v ~ /^[A-Z]/ ? Num[c] : Sym[c];
  if      (v ~ /+$/)  Y[c] = 1;
  else if (v ~ /-$/)  Y[c] = 0;
  else   X[c] }

function welford(c,v,     d) {
  v += 0;
  N[c]++;
  d      = v - Mu[c];
  Mu[c] += d / N[c];
  M2[c] += d*(v - Mu[c]);
  Sd[c]  = N[c] < 2 ? 0 : sqrt(M2[c] / (N[c] - 1));
  return v }

function disty(     d,n,c) {
  for(c in Y) { n += 1; d += (norm(c,$c) - Y[c])^2 }
  return sqrt(d/n) }

function norm(c,v) {
  return 1/(1 + exp(-1.7*(v - Mu[c]) / (Sd[c] + 1e-32)))  }

function bin(c,v) {
  if (v == "?") return v;
  if (c in Sym) return v;
  return int(BINS * norm(c,v)) }

function count(d,    klass,what,dn,c,v,bestrest) {
  klass = NF+1;          # where to store dist info
  d  = welford(klass, d); # computer dist, store its mean and Sd
  dn = norm(klass, d);   # map dist to the range 0..1
  bestrest = dn <= sqrt(NR-1)/(NR-1); # is this row close to heaven
  bestrest ? B++ : R++;
  for(c in X) {
    v = bin(c,$c);             # discretize
    Freq[c][v][bestrest]++ }} # update frequency counts

function report(    b,r,c,v,s,a,k,i,z) {
  for(c in Freq)
    for(v in Freq[c]) {
      b = Freq[c][v][1] / B;
      r = Freq[c][v][0] / R;
      s = b -r
      a[++i]["="] = s;
      a[  i]["col"] = c;
      a[  i]["val"]= v 
    }
  asort(a, z, "keyeq")
  for(i=1; i<=length(z); i++)
    print i, z[i]["="], Names[z[i]["col"]], z[i]["val"] }

function compare(a, b) { return a < b ? -1 : (a > b ? 1 : 0) }

function keyeq(i1, a1, i2, a2) { return compare(a2["="], a1["="]) }   

function rogues(    i,s,p) {
  p="^(NF|NR|FS|RS|RT|FNR|OFS|ORS|PREC|ARGC|ARGV|OFMT|LINT|FPAT|" \
    "ERRNO|RSTART|ARGIND|SUBSEP|CONVFMT|ENVIRON|SYMTAB|FUNCTAB|PROCINFO|" \
    "FILENAME|RLENGTH|BINMODE|IGNORECASE|FIELDWIDTHS|ROUNDMODE|TEXTDOMAIN)$"
  for(i in SYMTAB) if (i !~ p) printf " ?" i  > "/dev/stderr" }
