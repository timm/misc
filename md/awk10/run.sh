#!/usr/bin/env bash

Ext=${Awk10Ext:-md}
Lib=${Awk10Lib:-awk10/lib.awk}
Bin=${Awk10Bin:-$HOME/tmp/md/lib}

mkdir -p $Bin

parse() { gawk '
  /^@include/              { print "CODE "$0; next }
  /^(func|BEGIN|END).*}$/  { print "CODE "$0; next }
  /^(func|BEGIN|END)/,/^}/ { print "CODE "$0; next }
                           { print "DOC " $0} '
}
gen() { gawk '
  function prep(s) {
    print gensub(/\.([^0-9])([a-zA-Z0-9_]*)/,
                  "[\"\\1\\2\"]","g",s) }

  sub(/^DOC /,"#")         { print; next }
                           { gsub(/(CODE |[ \t]*$)/,"")   }
  /^@include/              { print $0; next }
  /^(func|BEGIN|END).*}$/  { prep($0); next }
  /^(func|BEGIN|END)/,/^}/ { prep($0); next }
                           { print "# " $0  } '
}

for i in *.${Ext}; do
  j=$Bin/${i%.*}.awk
  if [ "$i" -nt "$j" ]; then 
    cat $i | parse | gen > $j
  fi
done

j=$Bin/${1%.*}.awk

shift
AWKPATH="$AWKPATH:$Bin" gawk -f $Lib -f $j
