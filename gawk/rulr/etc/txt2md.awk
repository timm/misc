BEGIN {FS="\n"
        RS=""
       print "#!/usr/bin/env lua\n" }
$1 ~ /^ / {print(1); print "```lua\n'"$0"\n```\n"; next}
             {print "\n" $0 }
