src() { cat<<-'EOF' | sed -E 's/wme/names,nump,n,mu,m2,sd,has,acc/g'

BEGIN { main("header"); rogues() }

function main(go,   wme, n,acc) {
  while(getline>0) {
    gsub(/[ \t]*/,"")
    acc += @go($NF,++n > 20, wme)
    go="data" }
  return acc/n}

function header(_,_,     wme,i) {
 for(i=1;i<=NF;i++) {
   names[$i]
   if (i ~ /^[A-Z]/) nump[i] }}

function data(k,testp,     wme,i,out) {
  if (testp) out = k == test(wme)
  train(k,wme)
  return out }

function train(k, wme,i) {
  n[k]++
  for(i=1;i<NF;i++) 
    if ($i != "?") 
      i in nump ? num(k,i,$i,wme) : sym(k,i,$i,wme)}

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

EOF
}

gawk --source "`src`"
