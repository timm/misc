#!/usr/bin/env gawk -f
# vim: filetype=awk ts=2 sw=2 sts=2  et :

BEGIN      { FS="," }
NR == 1    { Want=NF
             for(i=1;i<=NF;i++)
               if ($i !~ /\?/)
                 use[++n]= i;
             print "want" Want
           }
NF == Want { s = $use[1]
             for(i=2;i<=n;i++) s = s "," $use[i] 
             print(s) }
NF != Want { msg= "'. Has "NF" fields, not "Want
             print "#W> Skipping '"$0 msg >"/dev/stderr" }

