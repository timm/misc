BEGIN {FS="\n"
        RS=""}

        NR==1{ next}
sub(/^    /,"") { gsub(/\n    /,"\n"); $0= "```lua\n"$0"\n```\n"}
          {print "\n\n" $0 }
