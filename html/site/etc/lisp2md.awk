BEGIN           { Top=1; print "\n\n[TOC]\n\n"}
$1 ~ /;include/ { print "\n\n```text"; include($2); print "```\n\n"; next; }
sub(/^#\|/,"")   { In=0;  print(Top ? "" : "```\n\n\n"); Top=0; next }
sub(/^\|#/,"")   { In=1;  print "\n\n\n```lisp"; next }
                { sub(/^;;-/,"-") 
                  print $0 }
sub(/\|#/,"")   { In=0;  next }

function include(x) {
  while ((getline < x) > 0) { print $0 }
  close(x) }
