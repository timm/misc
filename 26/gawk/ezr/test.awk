# test.awk: rank unseen rows by (xdist to best) - (xdist to rest);
# label the top Check of them, keep the one with least ydist.
# Metadata flows through (PASS=1) so xplain can draw its table.

BEGIN { FS = OFS = ","; PASS = 1 }

{ N++; Txt[N] = $0
  split($0, A, ",")
  Key[N] = xdist(A, Best) - xdist(A, Rest) }

END {
  chk = Opt["Check"] + 0; if (chk < 1) chk = 5
  besty = 2
  for (s=1; s<=chk && s<=N; s++) {     # next-best unused key
    lo = 0
    for (r=1; r<=N; r++)
      if (!Used[r] && (!lo || Key[r] < Key[lo])) lo = r
    Used[lo] = 1
    split(Txt[lo], A, ",")
    y = ydist(A)                       # the oracle peek
    if (y < besty) { besty = y; txt = Txt[lo] } }
  print "=picked", besty, txt }
