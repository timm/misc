NR==1{ for(i=1;i<=NF;i++) {
         names[i] = $i
         if ($i ~ /^[A-Z]/) {
             hi[i] = -big
             lo[i] =  big  }}}

function new(i) {i[0] = "\t"; del i[0] }

function num(i) { new[i]}
{ for(i in lo) {
     if ($i != "?") {
       n[i]++
       if ($i < lo[i]) lo[i] = $1
       if ($i > hi[i]) hi[i] = $1


  }}
