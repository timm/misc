# xplain.awk: the model is two centroids; show their gap,
# biggest first (what most swings this model's opinion of a row).

BEGIN { FS = "," }

$1 == "=picked" { Picked = $2 }

END {
  printf "picked %.2f\n\n", Picked
  printf "%6s%10s%10s  %s\n", "delta", "best", "rest", "attribute"
  for (c in Type) if (Role[c] == "x") {          # sort gaps, desc
    g = gap(c, Best[c], Rest[c])
    for (j=++n; j>1 && G[j-1]<g; j--) { G[j]=G[j-1]; C[j]=C[j-1] }
    G[j] = g; C[j] = c }
  for (j=1; j<=n; j++) { c = C[j]
    printf "%6.2f%10s%10s  %s\n",
           G[j], say(Best[c]), say(Rest[c]), Name[c] } }

function say(v) { return v+0 == v ? sprintf("%g", 0+sprintf("%.2f",v)) : v }
