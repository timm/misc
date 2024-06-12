
BEGIN             { code = 0 }

/^#output/        { print "```txt\n"; slurp($2 ".lua\n"); slurp( $2 ".out"); print "```\n"; next }
/^#include /      { print "```txt\n"; slurp($2); print "```\n"; next }
sub(/^```.*/,"")  { code = 1 - code }  
                  { print (code ? "" : "-- ")$0 }


function slurp(file,    s) { while((getline s <file)>0) print s }
