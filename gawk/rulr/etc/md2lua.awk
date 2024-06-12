
BEGIN             { code = 0 }

sub(/^```.*/,"")  { code = 1 - code }  
                  { print (code ? "" : "-- ")$0 }

