function cdf(z,    e,y) {
 y = 1.526 *z * (1 + 0.1034 * z)
 e = 106/39
 return 1/(1+e^(-y)) }

BEGIN{ for(i=-3;i<=3; i+= 0.1) print(i,cdf(i)); exit }

BEGIN { FS=","; Bins=5;
        Big=1E30 }
      { gsub(/[ \t]/,"") }
NR==1 { header() }
NR>1  { data()   }
END   { n=asort(D,D,"sort_d2h")
        for(i=1;i<=10;i++) {printf("%s ",d2h(D[i])); goals(D[i]) }
        print ""
        for(i=n-10;i<=n;i++) {printf("%s ",d2h(D[i])); goals(D[i]) }
        ranks()
        n=asort(Rank,Rank,"sort_d2h")}

function header(     col) {
  for(col=1;col<=NF;col++) {
    Name[col]=$col
    if ($col ~/-$/) Goal[col] = 0
    if ($col ~/+$/) Goal[col] = 1
    if ($col ~ /^[A-Z]/) {Hi[col]=-Big; Lo[col]=Big}}} 

function nump(col)  {return (col in Hi)   }
function goalp(col) {return (col in Goal) }

function data(     col) {
  for(col=1;col<=NF;col++) {
    if (nump(col)) {
      $col=$col+0
      if ($col> Hi[col]) Hi[col]=$col
      if ($col< Lo[col]) Lo[col]=$col }
    D[NR-1][col]=$col }}

function ranks(      n,row) {
  n = length(D)
  if (Rows > n/2) Rows=int(n/2) 
  for(row=1;      row<=Rows;row++) rank(row) 
  for(row=n-Rows;row<=n;    row++) rank(row) }

function rank(row,      n,col,x) {
  n = d2h(D[row])
  for(col in D[row]) 
    if (!goalp(col)) {
      x = D[row][col]
      if (x != "?") {
        x = (nump(col)) ? int(norm(col,D[row][col])*Bins) : x
        Rank[col,x]["col"]=col
        Rank[col,x]["x"]=x
        Rank[col,x]["score"] += n }}}

function norm(col,x) {
  return (x=="?") ? x : (x - Lo[col]) / (Hi[col] - Lo[col] + 1/Big) }
          
function d2h(row,     n,d) {
  d=0; n=0 
  for (col in Goal) {
    n++
    d=d + (Goal[col] - norm(col,row[col]))^2 }
  return (d/n)^.5 }

function sort_score(_,s1,__,s2)   { return cmp(s1["score"],s2["score"]) }
function sort_d2h(_,row1,__,row2) { return cmp(d2h(row1), d2h(row2)) }

function cmp(n1,n2) { return (n1 < n2) ? -1 : ((n1 == n2) ? 0 : 1) }

function goals(row,    i,sep) {
 for (i in Goal) {printf("%s%s=%s",sep,Name[i],row[i]); sep=", "}
 print ""}


