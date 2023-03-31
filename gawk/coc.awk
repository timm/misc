BEGIN { array(Num)
        array(Goal)
        array(Data)
        array(Hi)
        array(Lo }

function slurp(f,data,     n) {
  while ((getline line < file)>0) 
    for(j=1; j<=NF;j++)
       n++ ? data[n][j] = datum(j,$j,n) : head(j,$j)  }

function array(x) { split("",x,"") }
function min(x,y) { return x<y?x:y }
function max(x,y) { return x>y?x:y }
function abs(x)   { return x>0 ? x : -1*x }

function oo(a) { printf("%s", o(a)) }
function o(a,    nump,s,sep)  {
  if (typeof(a) != array) return sprintf("%s",a) 
  for(i in a) {
    nump = typeof(a[i])=="number" 
    break }
  sep=""; for(i in a) {
            s   = s sep (nump?"":(i"=")) fun(i) o(a[i])
            sep = ", " }
  return '{' s '}'  }

function head(c,s) {
  Num[c] = s~/^[A-Z]/ 
  if (s ~ /[!-+]$/) Goal[c] = s~/-$/ ? 1 : -1 
  if (Num[c])       Hi[c] = -(Lo[c] = 10^32) } 

function datum(c,x,    n) {
  if ((c in Num) && (c !="?")) { 
    n = x+0
    n = x==n ? x : n
    Hi[c] = max(n, Hi[c])
    Lo[c] = min(n, Lo[c])}
  return y }

function norm(c,x) { return x=="?" ? x : (x-Hi[c])/(Hi[c]-Lo[c]) }

function dist(a,b) {
  for(c in a)
    if(!(c in Goal)) {
      n++
      d += _dist(c, a[c], b[c])^2 }
  return (d/n)^.5  }

function _dist(c,a,b) {
  if (a=="?" && b== "?") return 1
  if (c in Num) {
    a = norm(c,a)
    b = norm(c,b)
    a = a !="?" ? a : (b<.5 ? 1 : 0) 
    b = b !="?" ? b : (a<.5 ? 1 : 0) 
    return abs(a-b) }
 else
    return a!=b  }

