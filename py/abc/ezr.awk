BEGIN {FS=","; P=2; D=0
      {gsub(/ \t\r/,"")
       split($0,a,FS)
       NR==1 ? head(a,++D): body(a,D,NR-1)}

function head(a,d,     i,s) {
  for(i in a) {
    s=a[i]
    Name[d,i]=s
    if (s !~ /X$/) {
      if (s ~ /+$/) Y[d][i]=1 
      if (s ~ /-$/) Y[d][i]=0 
      if (s ~ /^[A-Z]/) {Hi[d][i] = -(Lo[d][i] = 1E32)}}}}

function body(a,d,r,     i,x,y) {
  for(i in Hi) {
    y = 0 + (x = a[i])
    a[i] = x = (x==y ? y : x)
    if(x > Hi[i]) Hi[i]=x
    if(x < Lo[i]) Lo[i]=x
    Data[

