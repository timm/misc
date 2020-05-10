BEGIN{ T="Text\t:"; C="Code\t:"; X=T }
sub(/^--\]\][ ]*/,"")  { print X $0; X=C; next}
sub(/^[-]+     /,"")        { print C $0; next }
sub(/^--\[\[[ ]*/,"")  { print T $0; X=T; next}
sub(/^[-]+([ \t]*$| )/,"")    { X=T }
                       { print X $0 }
o

awk 'BEGIN          { In=0; Once=0 }
     NR < 4          { if (gsub(/^#/,"%")) {
                             print; next}}
    
     sub(/^--\[\[/,"") { if (Once)
                            print In ? "```" : "```lua"
                       
                       In = 1 - N; Once = 1 next
                     }
                     { print $0 }
     END { if (In) print "```\n\n"
           system("cat _footer.md")
           if (Ref ) 
              print "\n\nReferences\n==========\n\n" }
' $1  
