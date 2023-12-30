    { a[NR] = $0 }
END { for(i=1; i<=NR; i++) main(i, a[i]) }

function src(i)  { return a[i] ~ /^> /      }
function sp(i)   { return a[i] ~ /^[ \t]*$/ }

function main(i,s,    pre,post) {
   sub(/^> /,"",s)  
  if (src(i)   && sp(i-1)) print "```haskell\n" s
  if (src(i-1) && sp(i))   print s "```\n" }