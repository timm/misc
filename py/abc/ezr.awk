BEGIN {FS=","; P=2; D=0
      {gsub(/ \t\r/,"")
       split($0,a,FS)
       NR==1 ? head(a,++D): body(a,D)}

function head(a,d,     i) {
  for(i in a) {
    if (a[i]  ~/^[A-Z]/) {
      
