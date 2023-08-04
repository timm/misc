BEGIN           { Top=1; } #print "\n\n[TOC]\n\n"}
$1 ~ /;include1/ { print "\n\n```text"; include($2,1); print "```\n\n"; next; }
$1 ~ /;include/  { print "\n\n```text"; include($2); print "```\n\n"; next; }
sub(/^#\|/,"")   { In=0;  print(Top ? "" : "```\n\n\n"); Top=0; next }
sub(/^\|#/,"")   { In=1;  print "\n\n\n```lisp"; next }
                { sub(/^;;-/,"-") 
                  print $0 }
sub(/\|#/,"")   { In=0;  next }

function include(x, num,    n) {
  n=0
  while ((getline < x) > 0) { if (num) { printf("%4s.   s",++n,$0) } else { print($0)} }
  close(x) }
