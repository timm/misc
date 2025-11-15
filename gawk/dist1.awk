
BEGIN {srand(R?R:1) 
       pop(P,100,20)
       for(i in P) for(j in P) out[++n]= dist(P[i],P[j]) 
       asort(out)
       for(i=0;i<=n;i+= n/20) print(i, "   " ,out[i])
       poles(P)
}
function pop(p,m,n,   pos,i,j) {
  while(m--) {
    pos=rand()*10^6
    for(j=1;j<=n;j++) p[pos][j] = rand() }}

function abs(x)   { return x>0 ? x : -1*x }
function max(x,y) { return x>y ? x : y }
function o(a,   i,s,sep) {for(i in a) {s=s sep a[i]; sep=", "} print s}

function dist(x,y,    i,n,d) {
  for(i in x) {
    n += 1
    d += abs(x[i] - y[i])^2 }
  return d^.5/n^.5 }

function pairs(a,all,   i,j) {
  for(i in a) 
    for(j in a)
      if(i>j)
        print( dist(all[i], all[j]) ) }

function poles(all,     c,mu,n,i,any,lo,hi,some) {
  for(i in all) {
    any = any ? any : i
    lo  = far(any,all, .7, some)
    hi  = far(lo, all, .7, some) 
  }
  o(some)
  print(lo,hi)
  print(dist(all[lo],all[hi])) 
}

function far(i,all,gap,some,   most,out,j,n,mu) {
  for(j in all) {
    n++
    d = dist(all[i],all[j])
    mu += (d-mu)/n
    if (d> most) { most=d; out=j}
    if (d> (mu + (most - mu)*gap)) {some[i]=d; some[j]=d} }
    return out
}
