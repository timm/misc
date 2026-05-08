# metrics.awk -- pd, pf, prec, acc per class from "pred,actual" lines
# usage:  ... | gawk -f metrics.awk

BEGIN {
  FS = ","
  printf "\033[1;36m%5s %6s %6s %6s %6s  %s\033[0m\n",
         "n", "pd", "pf", "prec", "acc", "class"
}

{ CM[$2,$1]++; SEEN[$1] = SEEN[$2] = 1; N++ }

END {
  for (c in SEEN) score(c)
}

function score(c,    a, p, n, tp, fn, fp, tn, pd, pf, prec, acc) {
  for (a in SEEN) for (p in SEEN) {
    n = ((a SUBSEP p) in CM) ? CM[a,p] : 0
    if      (a == c && p == c) tp += n
    else if (a == c)           fn += n
    else if (p == c)           fp += n
  }
  tn   = N - tp - fn - fp
  pd   = (tp+fn) ? tp/(tp+fn) : 0
  pf   = (fp+tn) ? fp/(fp+tn) : 0
  prec = (tp+fp) ? tp/(tp+fp) : 0
  acc  = N ? (tp+tn)/N : 0
  printf "%5d %6.3f %6.3f %6.3f %6.3f  %s\n",
         tp+fn, pd, pf, prec, acc, c
}
