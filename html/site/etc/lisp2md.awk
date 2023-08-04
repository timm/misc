BEGIN           { Top=1; }
$1 ~ /;include/ { print "\n\n```text"; include($2); print "```\n\n"; next; }
sub(/^#\|/,"")  { In=0;  print(Top ? "" : "```\n\n\n"); Top=0; next }
sub(/^\|#/,"")  { In=1;  print "\n\n\n```lisp"; next }
sub(/\|#/,"")   { In=0 }
1

function include(x) {
  while ((getline y < x) > 0) print y
  close(x) }
