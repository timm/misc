# vim: ft=awk :

BEGIN {
  Code1= "\n\n<ul><details><summary><tt>"
  Code2= "</tt></summary>\n\n```awk\n"
  Code3= "\n```\n\n</details></ul>"
  RS=""; FS="\n"
}
/^#-/    { next }
/^# /    { Pre="## " }
/^## /   { Pre="### " }
/^### /  { Pre="#### " }
/^#### / { Pre="##### " }
         { print "\n" ($NF ~ /}[ \t]*$/ ? code($0) : $0) }

function code(x,    i,src,txt,sig,a,n,b,sep,name) { 
  name=src = $1
  gsub(/(function[ \t]*|\(.*)/,"",name)
  sig = name "("
  for(i=2;i<=NF;i++) {
    if(sub(/^[ \t]+##[ \t]*/,"",$i))
       txt=txt"\n"$i 
    else
        src=src"\n"$i }
  n = split($1,a, /[\(\)]/)
  split(a[2],b,/[ ,\t]+/)
  for(i=1;i<=length(b);i++) 
    if (b[i] ~ /:/) {
       sig = sig sep b[i]
       sep = ", " }
  return Pre name  txt Code1 sig ")" Code2 src Code3 }
