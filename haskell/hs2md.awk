function codep(i)  {return ((i in a) && (a[i] ~ /^> /))      }
function blankp(i) {return ((i in a) && (a[i] ~ /^[ \t]*$/)) }

{a[NR]=$0}
END {for(i=1;i<=NR;i++) {  
      pre = post = ""
      if (codep(i)   && blankp(i-1)) pre="```haskell\n"
      if (codep(i-1) && blankp(i))   post="```\n"
      s=a[i]; sub(/^> /,"",s)  
      print(pre s post)}}
