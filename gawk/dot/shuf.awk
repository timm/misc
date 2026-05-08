# shuf.awk -- shuffle CSV rows, keep header (row 1) in place
# usage:  gawk -v seed=N -f shuf.awk data.csv

BEGIN { srand(seed + 0) }

NR == 1 { print; next }
        { a[++n] = $0 }

END {
  for (i = n; i > 1; i--) {
    j = int(rand() * i) + 1
    t = a[i]; a[i] = a[j]; a[j] = t
  }
  for (i = 1; i <= n; i++) print a[i]
}
