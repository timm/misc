# cook.awk: discretize once, up front. Each row prints as a
# two-line paragraph: raw, then cooked (NUMs -> bins 0..6 via
# int(7*norm()); SYMs and "?" pass through). Downstream, all
# distance math collapses to integer compares, and per-bin y
# stats (ranges!) become one array index away.

BEGIN { FS = OFS = "," }

{ sub(/\r$/, "") }                     # some moot files are CRLF

NR == 1 { NCol = NF
          for (i=1; i<=NF; i++) { n = $i
            if (n ~ /X$/) continue
            Type[i] = (n ~ /^[A-Z]/) ? "NUM" : "SYM"; Name[i] = n }
          print; next }

{ R++; for (i=1; i<=NF; i++) Data[R][i] = $i }

END {
  for (c in Type) if (Type[c] == "NUM") {        # column stats
    s = q = n = 0
    for (r=1; r<=R; r++) { v = Data[r][c]
      if (v != "?") { s += v; q += v*v; n++ } }
    Mu[c] = s/n; Sd[c] = sqrt((q - s*s/n) / (n-1)) }
  for (r=1; r<=R; r++) {
    raw = cooked = ""
    for (i=1; i<=NCol; i++) { v = Data[r][i]
      raw    = raw    (i>1 ? "," : "") v
      cooked = cooked (i>1 ? "," : "") bin(i, v) }
    print raw; print cooked; print "" } }

function bin(c,v,   b) {
  if (v == "?" || Type[c] != "NUM") return v
  b = int(7 * norm(c, v)); return b > 6 ? 6 : b }
