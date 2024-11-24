src() { cat<<-'EOF' | sed -E 's/wme/n,mu,m2,sd,has,rows,nump,names/g'

BEGIN { K=1
        M=2
        main("header"); rogues() }

function main(go,     wme,nn,acc) {
  while(getline>0) {
    gsub(/[ \t]*/,"")
    acc += @go($NF, ++nn, wme)
    go = "data" }
  return acc/(nn - 20) }

function header(_,__,     wme,i) {
 for(i=1;i<=NF;i++) {
   names[$i]
   if (i ~ /^[A-Z]/) nump[i] }}

function data(k,nn,     wme,i,out) {
  if (nn > 20) out = k == likes(wme,nn,length(n))
  train(k,wme)
  return out }

function train(k, wme,i) {
  n[k]++
  r = length(r
  for(i=1;i<NF;i++) {
    x = $i+0
    x = x==$i ? x : $i
    if ($i != "?") 
      i in nump ? num(k,i,$i,wme) : sym(k,i,$i,wme)}

function likes(wme, nall, nh) {
  for(k in n) {
    prior = (length(rows) + K)/ (nall+ K*nh)
    for(i=1;i<NF;i++) {
      if  
}
function sym(k,i,x,wme) { ++has[k][i][x] }

function num(k,i,x,wme,      d,nn) {
  nn        = N[k]
  d         = x - mu[k][i] 
  mu[k][i] += d/nn
  m2[k][i] += d*(x  - mu) 
  if (n >=2)
    sd[k][i] = (m2[k][i]/(nn - 1))^.5  }

function rogues(   i) {
  for(i in SYMTAB) if (i ~ /^[a-z]/) print("?",i, typeof(SYMTAB[i])) }

function isNum(s) { return s+0 == s }

function o(a,  pre,   j,k,s,sep,keyp) {
  if (typeof(a) == "number") return sprintf("%g",a)
  if (typeof(a) != "array" ) return a
  for(j in a) { keyp = !isNum(j); break }
  for(j in a) {
    k = keyp ? j"=" : ""
    s = s sep k o(a[j])
    sep = ", " }
  return pre "(" s ")" }

EOF
}

if [[  -t 0 ]]; then
  gawk --source "`src`" $*
else 
  cat - | gawk --source "`src`" $*
fi

