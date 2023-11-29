function trim(s) { sub(/^[ \t]*/,"",s); sub(/[ \t*]$/,"",s); return s}

function per(a,p)  { return a[int(p*length(a))] }
function median(a) { return per(a,.5)    }
function sd(a)     { return (per(a,.9) - per(a,.1)) / 2.56 }

function mode(a,    most,out) {
  for (i in a) if (a[i] > most) {most=a[i]; out=i}
  return out }

function ent(a,   N,e) {
  for(i in a) N += a[i]
  for(i in a) e  = e - a[i]/N * log(a[i]/N)/log(2)
  return e }
