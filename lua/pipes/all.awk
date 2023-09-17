BEGIN {FS=","; BIG=1E30}
NR==1 { head() }
NR> 1 { body(rand()) }
END   {for(i in d) go(n,d[i],d)}

function head(    i) {
  for(i=1;i<=NF;i++) {
    Names[i] = $i
    if ($i ~ /^[A-Z]) {
      lo[i] =   BIG
      hi[i] = - BIG }}}
       
function body(pos,      i,x) {
  for(i=1;i<=NF;i++) {
    x=$i
    d[pos][i] = x==(x+0) ? x+0 : $i
    train(}}

function has(x,k) { k=k==""?length(x)+1:k; x[k][1]; del x[k]; return k}
function klass(i) {
  has(i,"mu")
  has(i,"m2")
  has(i,"sd")
  i["n"]  = 0
  i["lo"] = BIG
  i["hi"] = - BIG
}
train
function train(k,all,now,      x,i,d) {
  if (! (k in all)) {
    has(all,k)
    klass(all[j]) }
  train1(k,all[j],now) }

function train1(k,one,now) {
  for(i in lo) 
    x = now[i]
    if (x != "?") {
      one[
      one["n] N[k][i]++
      Lo[k][i] = x < Lo[k][i] ? x : Lo[k][i]
      Hi[k][i] = x > Hi[k][i] ? x : Hi[k][i]
      d        = x - Mu[k][i]
      Mu[k][i] = Mu[k][i] + d/N[k][i]
      M2[k][i] = M2[k][i] + d*(x - Mu[k][i])
      if (N[k][i] > 1) 
        Sd[k][i] = (M2[k][i]/(N[k][i] - 1))^.5 }}}

function go(n, now, all,    i) {
  for(i in lo) {
    x = now[i]
    if (x!="?")

}

