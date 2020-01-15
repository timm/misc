#!/usr/bin/env ../fun
# vim: filetype=awk ts=2 sw=2 sts=2  et :

     { gsub(/[\r\t ]*/,"",$0) 
       sub(/#.*/,"",$0)
     }
/^$/ { next }
/,$/ {  b4 = b4 $0; next  }
     {  print b4 $0; b4="" }
