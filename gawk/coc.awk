BEGIN { array(Num)
        array(Goal)
        array(Data)
        array(Hi)
        array(Lo }

function slurp(f,data,     n) {
  while ((getline line < file)>0) 
    for(j=1; j<=NF;j++)
       n++ ? data[n][j] = datum(j,$j,n) : head(j,$j)  }

function array(x { split(""x,"") }
function min(x,y) { return x<y?x:y }
function max(x,y) { return x>y?x:y }

function head(c,s) {
  Num[c] = s~/^[A-Z]/ 
  if (s~/[!-+]$/)
    Goal[c] = s~/-$/ ? 1 : -1 
  if (Num[c]) 
    Hi[c] = -(Lo[c] = 10^32) } 

function datum(i,x) {
  if (c in Num) 
if (x=="?") {
  n = x+0
  n = x==n ? x : n
  Hi[i] = max(n, Hi[i])
  so[i] = min(n, Lo[i] )}
  return y }

function dist(a,b)

