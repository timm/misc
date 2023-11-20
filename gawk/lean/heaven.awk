BEGIN {FS=","; big=1E30}
NR==1 { 
  for(i=1;i<=NF;i++) {
    if ($i ~ /^[A-Z]) num[i] 
    if ($i ~ /-$) w[i]=0
    if ($i ~ /+$) w[i]=1 }
  for(i in w) {
      lo[i]=  big
      hi[i]= -big }}
NR>1 { 
  for(i=1;i<=NF;i++) {
    if (i in num) $i+=0
    row[NF-1][i] = $i
    if (i in w) {
    if ($i < lo[i]) lo[i]=$i
    if ($i > hi[i]) hi[i]=$i }}}

function norm(i,x) { return (x-lo[i]) / (hi[x] - lo[x] + 1/big) }
function d2h(i,x)  { return abs(w[i] - norm(i,x) }
function d2hs(i,a,    n,d) {
  for(i in w) {
    n+=1
    d+= d2h(i,a[i])^2 }
 return (d/n)^.5

function sortByD2h(_,r1,__,r2) {
  if d2hs(r1) < d2hs(r2) return -1
#---
function abs(x) { return x<0 ? -x : x }
