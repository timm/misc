BEGIN { FS=","
        array(Num)
        array(Name)
        array(X)
        array(Y)
        array(Hi)
        array(Lo) 
        CONVFMT = "%.2f"
        knowns() 
        main() }
#----------------------------------------------------------
function main(      r,a) {
  slurp("/Users/timm/gists/data/auto93.csv",Data)
  for(r in Data) {
    if(r>10) break
    print("\n" o(Data[1]))
    print(o(Data[r]),r,dist(Data[1],Data[r])) }
  stats(Data,Y,"div",a); oo(a)
  for(r in Data) rows[r]=r
  n=asort(rows,rows1,"betters")
  for(i=1;i<=n;i++)
    
     
  unknowns() }
#----------------------------------------------------------
function slurp(file,data,     n,c) {
  n=-1
  while (getline < file) {
    n++
    for(c=1; c<=NF;c++) 
      n==0 ? header(c,$c) : (data[n][c] = cell(c,$c)) }
  close(file) }

function header(c,s) {
  Name[c] = s
  (s ~ /[-\+!]$/)  ?  (Y[c] = s~/-$/ ? 1 : -1) : (X[c]=c)
  if (s ~ /^[A-Z]/) {
    Num[c]
    Hi[c] = -(Lo[c] = 10^32) }} 

function cell(c,x) {
  if ((c in Num) && (c !="?")) { 
    x = x+0
    Hi[c] = max(x, Hi[c])
    Lo[c] = min(x, Lo[c]) }
  return x }
#----------------------------------------------------------
function stats(rows,cols,fun,a,     c) {
  fun = fun ? fun : "mid"
  array(a)
  for (c in cols) 
    a[Name[c]]= @fun(c,rows)
  a["N"] = length(rows) }

function nums(c,rows,a,     r,x) {
  for(r in rows) { x= Data[r][c]; if(x != "?")  a[r]=x }
  return asort(a) }

function syms(c,rows,a,     r,x) {
  for(r in rows) { x= Data[r][c]; if(x != "?")  a[x]++ }}

function div(c,rows,    n,a,p,e,x) {
  if (c in Num) {
    n = nums(c,rows,a)
    return (a[int(n*.9)] - a[int(n*.1)]) / 2.58 
  } else {
    syms(c,rows,a)
    for(x in a) {
      p  = a[x]/length(rows)
      e -= p * log(p)/log(2) }
    return e }}
   
function mid(c,rows,  n,a,x,mode,most) {
  if (c in Num) {
    n = nums(c,rows,a)
    return a[int(n*.52)] 
  } else {
    syms(c,rows,a)
    for(x in a) 
      if (a[x] > most) { most=a[x]; mode=x }
    return mode } }
#----------------------------------------------------------
function better(a,b) {
  e= 2.71828
  n= length(Y)
  for(c in Y) {
    w   = Y[c]
    a   = norm(c,a[c])
    b   = norm(c,b[c])
    s1 -= e^(w*(a - b)/n)
    s2 -= e^(w*(b - a)/n) }
  return s1/n < s2/n }

function betters(_,a,__,b) {
  return better(Data[a],Data[b]) ? -1 : 1 }
#----------------------------------------------------------
function dist(a,b,    c,d,n) {
  for(c in a)
    if(!(c in Y)) {
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

function norm(c,x) { return x=="?" ? x : (x-Lo[c])/(Hi[c]-Lo[c]) }
#----------------------------------------------------------
function array(x) { split("",x,"")        }
function min(x,y) { return x<y ? x : y    }
function max(x,y) { return x>y ? x : y    }
function abs(x)   { return x>0 ? x : -1*x }

function simple(a,   i) {
  for(i in a)
     if (! ((i==1) || ((i-1) in a))) return 0 
  return 1 }

function oo(a,pre) { print(o(a,pre)) }

function o(a,pre,   c,s,sep,keys)  {
  if (typeof(a)!="array") return a
  keys=!simple(a) 
  sep=""
  for(c in a) {
    s   = s sep (keys ? c"=": "") o(a[c],"")
    sep = ", " }
  return pre "{" s "}"  }

function knowns(      s) { KNOWN["KNOWN"]; for(s in SYMTAB) KNOWN[s] }
function unknowns(    s) { 
  for(s in SYMTAB) 
    if (s ~ /^[A-Z]/)  
      if (! (s in KNOWN))
         print "#W> Global: " s>"/dev/stderr" 
  for(s in SYMTAB) 
    if (s ~ /^[_a-z]/) 
      print "#W> Rogue: "  s>"/dev/stderr" }
