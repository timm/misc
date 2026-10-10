# bestRest.awk: csv in; metadata + centroids + test rows out.
# Shuffle, train on half: label Budget-Check rows, sort by ydist,
# sqrt best, rest rest; emit both centroids and the unseen half.

BEGIN { FS = OFS = ","
        Budget = Budget ? Budget : 50
        Check  = Check  ? Check  : 5
        srand(Seed ? Seed : 1) }

{ sub(/\r$/, "") }                     # some moot files are CRLF

NR == 1 { NCol = NF
          for (i=1; i<=NF; i++) { n = $i
            if (n ~ /X$/) continue
            Type[i] = (n ~ /^[A-Z]/) ? "NUM" : "SYM"
            Name[i] = n
            if (n ~ /[-+!]$/) { Role[i]="y"; Goal[i] = n~/-$/ ? 0:1 }
            else                Role[i]="x" }
          next }

{ R++; for (i=1; i<=NF; i++) Data[R][i] = $i }

END {
  for (c in Type) if (Type[c] == "NUM") {        # column stats
    s = q = n = 0
    for (r=1; r<=R; r++) { v = Data[r][c]
      if (v != "?") { s += v; q += v*v; n++ } }
    Mu[c] = s/n; Sd[c] = sqrt((q - s*s/n) / (n-1)) }

  for (r=1; r<=R; r++) P[r] = r                  # shuffle
  for (r=R; r>1;  r--) { j = int(rand()*r)+1
                         t=P[r]; P[r]=P[j]; P[j]=t }

  half = int(R/2)
  nlab = Budget - Check; if (nlab > half) nlab = half
  for (r=1; r<=nlab; r++) {                      # sort by ydist
    y = ydist(Data[P[r]])
    for (j=r; j>1 && Yv[j-1]>y; j--) { Yv[j]=Yv[j-1]; L[j]=L[j-1] }
    Yv[j] = y; L[j] = P[r] }

  k = int(sqrt(nlab))                            # sqrt best, rest rest
  centroid(L, 1,   k,    CB)
  centroid(L, k+1, nlab, CR)

  print "=opt", "Check", Check
  for (c=1; c<=NCol; c++) if (c in Type)
    print "=col", c, Name[c], Type[c], Role[c], Mu[c], Sd[c], Goal[c]
  print "=best" row(CB)
  print "=rest" row(CR)
  for (r=half+1; r<=R; r++) print substr(row(Data[P[r]]), 2) }

function centroid(L,lo,hi,Cen,   c,i,v,n,s,top,cnt) {
  for (c in Type) {
    delete cnt; n = s = 0; top = ""
    for (i=lo; i<=hi; i++) { v = Data[L[i]][c]
      if (v == "?") continue
      if (Type[c] == "NUM") { s += v; n++ }
      else if (++cnt[v] > cnt[top]) top = v }
    Cen[c] = Type[c]=="NUM" ? (n ? s/n : "?") : (top=="" ? "?":top) } }

function row(A,   c,s) {                         # full-width csv
  for (c=1; c<=NCol; c++) s = s "," (c in A ? A[c] : "?")
  return s }
