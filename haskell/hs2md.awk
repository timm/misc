    { a[NR] = $0 }
END { for(i=1; i<=NR; i++) main(i, a[i]) }

function src(i)  { return a[i] ~ /^> /      }
function sp(i)   { return a[i] ~ /^[ \t]*$/ }

function main(i,s,    pre,post) {
   sub(/^> /,"",s)  
  if (src(i)   && sp(i-1)) pre= v"```haskell\n" 
  if (src(i-1) && sp(i))   post= "```\n" 
  print pre s post }