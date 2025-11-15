
BEGIN {srand(R) ; pop(P,500,20)
       #xx(P)
       for(i in P) for(j in P) out[++n]= dist(P[i],P[j]) 
       asort(out)
       for(i=0;i<=n;i+= n/20) print(i, "   " ,out[i])
       print far(P,Q)
       for(i in Q) for(j in Q) if(i > j) print("!!",i,j,dist(P[i],P[j]))
}
function pop(p,m,n,   pos,i,j) {
  while(m--) {
    pos=rand()*10^6
    for(j=1;j<=n;j++) p[pos][j] = rand() }}

function abs(x)   { return x>0 ? x : -1*x }
function max(x,y) { return x>y ? x : y }

function dist(x,y,    i,n,d) {
  for(i in x) {
    n += 1
    d += abs(x[i] - y[i])^2 }
  return d^.5/n^.5 }

function xx(p,     i,j,n) { 
  n=50
  for(i in P)
    for(j in P) {
      if (n-- < 0) return 0
        if(i!=j)
          print(dist(P[i],P[j])) }}

function o(a,      i,s,sep) {
  for(i in a) {s = s sep a[i]; sep=", "}
  print(s)
}
function pairs(a,all,   i,j) {
  for(i in a) 
    for(j in a)
      if(i>j)
        print( dist(all[i], all[j]) )
}
function fastmap(all) {
  for(i in all)
    any = any ? any : i
    lo  = far(any,all, .8, some)
    hi  = far(lo, all, .8, some) 
  }
  o(some)
  print(dist(all[lo],all[i])
}
function far(j,all,gap,some,   i,n,mu) {
   for(i in all) {
     n++
     d = dist(all[i],all[j])
     mu += (d-mu)/n
     if (d> most) { most=d; out=i}
     if (d> (most - (most - mu)*gap)) {some[i], some[j]}
   }


function far1(near,stop,b4,all,n,ds,most,j,k,out,      i,d1,d2,after) {
  for(i in b4) {
    any    = any or i
   
    d1   = dist(all[i],all[j])
    d2   = dist(all[i],all[k])
    if (abs(d1 - d2) > near*most) after[i]
    if (d1>most||d2>most) {
        if (d1> most) k=i
        if (d2> most) j=i 
        after[i]
        most=max(d1,d2) }
  }
  print(most,length(after))
  if (length(after) && length(after) < length(b4)*stop)
     return far1(near,stop,after, all,most,j,k,out)
  else {
     out[]
     out[k]
     for(i in b4) out[i]
     return most}
}
