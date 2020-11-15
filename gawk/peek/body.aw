BEGIN {
  Code1= "\n\n<ul><details><summary><tt>"
  Code2= "</tt></summary>\n\n```awk\n"
  Code3= "\n```\n\n</details></ul>\n\n"
}
END { print "\n=============================\n" Toc 
      rogues()
}
NR==1              { next } 
sub(/^#:/     ,"") { print till($0, "^:#"); next }
sub(/^## /    ,"") { print code(); next }
sub(/^# # /   ,"") { print "\n# " $0   ; In="";         Pre="\n## "   ; Toc=Toc "\n- ["$0"](#"$0")"; next}
sub(/^# ## /  ,"") { print "\n## " $0  ; In="   ";      Pre="\n### "  ; Toc=Toc "\n  - ["$0"](#"$0")"; next}
sub(/^# ### / ,"") { print "\n### " $0 ; In="      ";   Pre="\n#### " ; Toc=Toc "\n    - ["$0"](#"$0")"; next}
sub(/^# #### /,"") { print "\n#### " $0; In="        "; Pre="\n##### "; Toc=Toc "\n      - ["$0"](#"$0")"; next}
sub(/^# /,"")      { print $0 }

function trim(s) {
  gsub(/^[ \t]*/,"",s)
  gsub(/[ \t]*$/,"",s)
  return s }

function lines(s,a, i) { 
  gsub(/\n##[ \t]*/,"",s); split(s,a,"\n"); return s}

function code(      name0,com,src,out,i,srcs,coms) {
  com  = till($0, "^function") 
  com  = lines(com,coms); #print NR,"+", $0,"COM(" com ")\n";  
  src  = till($0, "^[ \t]*$")
  src  = lines(src,srcs); #print NR,"++",$0,"SRC(" src ")\n"
  name0= name(srcs[1])
  out  = (Pre ? Pre : "## ") name0 "\n" com
  Toc  = Toc "\n"In"- ["name0"](#"tolower(name0)") "coms[2]
  print out Code1 sig(srcs[1]) Code2 src Code3 
  }

function o(a,p,    i) { for(i in a) print p a[i] }

function name(s,a) {
  split(s, a, /[ \t\(\),]/)
  return a[2] }

function sig(s,   a,i,out,sep,n) {
  n = split(s, a, /[ \?\t\(\),]/)
  for(i=3;i<=n;i++) {
    if (a[i] !~ /:/) break
      out = out sep a[i]
      sep = ", "
  }
  return "<tt>" a[2] "(" out ")</tt>" }

function till(x,p,      sep,out) { 
  sep=""
  do  {
    gsub(/^# / ,"",x)
    gsub(/^## /,"",x)
    out =  out sep x
    sep = "\n"
    getline
    x=$0
  } while(x !~p)
  return out }

function rogues(   s) { 
  for(s in SYMTAB) if (s ~ /^[_a-z]/) print "#W> Rogue: "  s>"/dev/stderr"
}

