    { a[NR] = $0 }
END { for(i=1;i<=NR;i++) main(i,a[i]) }

function src(i)  { return a[i] ~ /^> /      }
function sp(i)   { return a[i] ~ /^[ \t]*$/ }

function main(i,s,    pre,post) {
  if (src(i)   && sp(i-1)) pre="```haskell\n"
  if (src(i-1) && sp(i))   post="```\n"
  sub(/^> /,"",s)  
  print(pre s post)  }