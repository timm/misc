"""
function showTwo(i,x,y) { print x,y }
function my(k,f,    m) {
	  while(k) {m=k f;if (m in FUNCTAB) MY=m; return};k=ISA[k]}
	    print "E> failed method lookup on ["f"]"; exit 1
    }
    function isa(x,y,_) { ISA[x]=y }
    function Num() {
	      isa(Object(i,j,k))
      }
      BEGIN {
	      f="show"
	      n=10


	      for (i = 1; i <= n; i++)
		          @Two(i,k,k)
      }

 """
 
 
 #
 # catch the current function name (needed for isa)
/^[ \t]*function[ \t]+/ {
	Fun=gensub(/^[ \t]*function[ \t]+([_A-Za-z0-9]+).*/,"\\1","g",$0)	
}
# make isa also add links kids to parents
/^[ \t]*isa\(/ {
	$0= gensub(/(^[ \t]*)isa\((([_A-Za-z0-9]+)\(([^,\)]+).*)/,
	             "\\1isa(\""Fun"\",\"\\3\",\\2","g",$0)
}
# add polymoprhism lookup
/@[A-Z]/ {
	$0= gensub(/@([A-Z].+)(\(([^,\)]+)[,\)])/, "my(\\3,\"\\1\")@MY\\2","g",$0)
}
1
