BEGIN          { Top=1 }
sub(/\#\|/,"") { In=0; print(Top ? "" : "```\n\n\n"); Top=0; next }
sub(/\|\#/,"") { In=1; print "\n\n\n```lisp"; next }
               { sub(/^;;-/,"-") 
                 print In " " $0 }
END            { if (In) print "```\n\n"}
