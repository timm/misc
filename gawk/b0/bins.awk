BEGIN { Bins=7 }

function chop(a,x,           y,lo, hi,mid)  {
  lo = 1
  hi = length(a)
  while (lo <= hi) {
    mid = int((hi + lo) / 2)
    y = a[mid]
    if (x == y) break
    if (x <  y) {hi=mid-1} else {lo=mid+1} }
  return mid }

function bin(j,a, x) {
  if (x=="?") return x
  if (!(j in isNum)) return x
  if (length(a) <= Bins) return x
  m = int(length(a)/Bins)
  pos = int(chop(a,x)/m) * m
  return a[pos] }

NR==1 {for(i=1;i<=NF;i++) {
         if ($i ~ /^[A-Z]*/) isNum[i]
         $i ~ /[!+-]$/ ? y[i] : x[i] }}

NR>1 {for(i=1;i<=NF;i++) row[NR-1][i] = $i 
      for(i in x) col[i][NR-1] = $i }

END {for(i in x) asort(col[i])
     for(i in row) {
       printf(bin(j,col[1],row[i][1]))
       for(j=2;j<=NF;j++) printf(", "bin(j,col[j],row[i][j]))
       print("") }}
       
