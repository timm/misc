BEGIN {
  Code1= "\n\n<ul><details><summary><tt>"
  Code2= "</tt></summary>\n\n```awk\n"
  Code3= "\n```\n\n</details></ul>"
  RS=""; FS="\n"
}
/^#-/              { next }
$NF ~ /}[ \t]+$/   { print "CODE " $0}

sub(/^# # /   ,"") { $1=trim($1); R[++N]=$0; In="- ";      Pre="\n## "  ; print In,"["$1"](#"$1")"; next}
sub(/^# ## /  ,"") { $1=trim($1); R[++N]=$0; In="  - ";    Pre="\n### " ; print In,"["$1"](#"$1")"; next}
sub(/^# ### / ,"") { $1=trim($1); R[++N]=$0; In="    -";   Pre="\n#### " ; print In,"["$1"](#"$1")"; next}
sub(/^# #### /,"") { $1=trim($1); R[++N]=$0; In="      -"; Pre="\n##### "; print In,"["$1"](#"$1")"; next}
/^function/        { print toc($1,$2); R[++N] = code($0); next}
                   { sub(/^# /,"x")    
                     R[++N] = $0 }
END                { for(I in R) print "\n"R[I] 
                     rogues() }

function toc(head,comment,   a,out) {
  split(head, a, /[ \(]/)
  out = "      - [" a[2] "](#" a[2] ")"
  return out (gsub(/^[ \t]*##/,"",comment) ? " : " comment : "") }

function code(x,    i,src,txt,sig,a,n,b,sep,name) { 
  name=src = $1
  gsub(/(function|\(.*)/,"",name)
  for(i=2;i<=NF;i++) {
    if (sub(/^[ \t]+##/,"",$i) ) { txt = txt "\n" $i }
    else                         { src = src "\n" $i }}
  n = split($1,a, /[\(\)]/)
  sub(/function /,a[1])
  sig = name "("
  split(a[2],b,/[ ,\t]+/)
  for(i=1;i<=length(b);i++) 
    if (b[i] ~ /:/) {
       sig = sig sep b[i]
       sep = ", "
  }
  return Pre name  txt Code1 sig ")" Code2 src Code3 }

function trim(s) {
  gsub(/^[ \t]*/,"",s)
  gsub(/[ \t]*$/,"",s)
  return s }

function rogues(   s) { 
  for(s in SYMTAB) if (s ~ /^[_a-z]/) print "#W> Rogue: "  s>"/dev/stderr"
}

