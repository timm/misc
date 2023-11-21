BEGIN {FS=","; Big=1E30; Best=.1}
      {NR==1 ? head() ; body()}
END   {main()}

function main(      i,n,m) {
  for(i in Row) 
     Row[i]["d2h"] = d2hs(Row[i]["cells"])
   n=asort(Row,Row1,"sortByD2h")}
   for(i in Row1)
     
    

function head(      i) {
  for(i=1;i<=NF;i++) {
    if ($i ~ /^[A-Z]) Num[i] 
    if ($i ~ /-$) W[i]=0
    if ($i ~ /+$) W[i]=1 }
  for(i in W) {
      Lo[i]=  Big
      Hi[i]= -Big }}

function body(           i) {
  for(i=1;i<=NF;i++) {
    if (i in Num) $i+=0
    Row[NF-1]["cells"][i] = $i
    if (i in W) {
      if ($i < Lo[i]) Lo[i]=$i
      if ($i > Hi[i]) Hi[i]=$i }}}

function d2h(i,x)  { return abs(W[i] - norm(x,Lo[i],Hi[i]) }
function d2hs(a,    n,d) {
  for(i in W) {
    n+=1
    d+= d2h(i,a[i])^2 }
 return (d/n)^.5 }

function sortByD2h(x,_,,y,__) {
  return compare(Row[x]["d2h"],Row[y]["d2h"]) }

#-----------------------------------
function compare(x,y) {
  if (x < y)  return -1
  if (x==y )  return  0
  if (x > y) return  1 }

function norm(x,lo,hi) { return (x-lo) / (hi- lo + 1/Big) }
function abs(x)        { return x<0 ? -x : x }
