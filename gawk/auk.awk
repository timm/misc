BEGIN {In=1}
gsub(/^"""/,"") { In =  1 - In  }
                   { pre  = In ? "" : "# " }
In {
    print pre gensub(/\.([^0-9])([a-zA-Z0-9_]*)/, 
                    "[\"\\1\\2\"]", "g", $0)
    next }
   {
    print pre $0
   }

