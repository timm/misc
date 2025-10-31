BEGIN {_ = "\t"}
funtion add(a,s, b) {
  split(s,b,_)
  n=length(a) + 1
  for(i in b) a[n][i] = b[i] }

function NUM(i,s) { return "num" _ i _ s _ "0 0 0" _ (s !~ /-$/) }

BEGIN { OFS=","; print(NUM(1,"asdas"))}
