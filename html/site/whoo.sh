#!/usr/bin/env bash
# vim: set ts=2 sw=2 expandtab :
NEEDS="gawk vim"


# --------------------------------------------------------------
_install() {
  if   [[ "$OSTYPE" == "darwin"* ]]   ; then brew install $1
  elif [[ "$OSTYPE" == "linux-gnu"* ]]; then apt-get install $1
  elif [[ "$OSTYPE" == "freebsd"* ]]  ; then pkg install $1
  elif [[ "$OSTYPE" == "cygwin" ]]    ; then echo eek, how do i install $1
  elif [[ "$OSTYPE" == "msys" ]]      ; then echo eek, how do i install $1
  elif [[ "$OSTYPE" == "win32" ]]     ; then echo eek, how do i install $1
  else                                       echo eek, how do i install $1
  fi
}
list2md() { gawk '
  BEGIN           { Top=1; }
  $1 ~ /;include/ { print "\n\n```text"; include($2); print "```\n\n"; next; }
  sub(/^#\|/,"")  { In=0;  print(Top ? "" : "```\n\n\n"); Top=0; next }
  sub(/^\|#/,"")  { In=1;  print "\n\n\n```lisp"; next }
  sub(/\|#/,"")   { In=0 }
  1

  function include(x) {
    while ((getline y < x) > 0) print y
      close(x) } ' ;}

for c in $NEEDS; do
  which $c >/dev/null || _install $c ;done

_gawk1(){ gawk '{print 22, $0}'  ;}

cat a.txt | _gawk1 

