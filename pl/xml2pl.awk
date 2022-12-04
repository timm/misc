# splot/splot-research/WebContent/models
/
BEGIN {FS="\t"; last=0}
function left()  { In++; printf "["}
function right() {for(i=1;i<=(In-NF);i++) printf "]"; print ""
	        In=NF}

/^<feature_tree>/,/<\/feature_tree/ {
	if (sub(/^:r/,"")) {left();  print  $0 "," } 
	if (sub(/^[\t ]*:m/,"")) {left();  printf "must,"   $0 "," } 
	if (NF < last)  right() 
	last=NF
	
}
