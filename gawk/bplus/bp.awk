BEGIN { FS=","
        B=12
        G=2    # Good = frist 2 bins
        M=1
        K=2 }

{ gsub(/[ \t\r]/,""); NR==1 ? head() : body(NR-1) }

function head(    i) {
  for(i=1;i<=NF;i++) {
    Name[i] = $i
    if ($i~/^[A-Z]/) 
      Hi[i] = -(Lo[i]=1E32) 
    if ($i !~ /X$/) 
      ($i ~ /[^+-!]$) ? X[i] : Y[i] = $i ~ /\+$/ }}

function body(r,     i) {
  for(i=1;i<=NF;i++) {
    $i = ($i != "?" && i in Hi) ? anotherNumber(i,$i) : $i
    Row[r[i]=$i }
  y = anotherNumber(NF+1, ydist(row))
  for(i in Row[r])
    F[y<=G][i][ Row[r][i] ]++ }

function anotherNumber(i,v) {
  v += 0
  if (v > Hi[i]) Hi[i]=v
  if (v < Lo[i]) Lo[i]=v
  gap = (Hi[i] - Lo[i]) / B
  return Lo[i] + gap * int(0.5 + (v- Lo[i]) / gap)}

function ydist(row,    y,n,d) {
  for(y in Y) {
    n++
    d += abs(norm(y,row[y]) - Y[y])^2}
  return (d/n) ^ 0.5 }

function norm(c,x) {
  return x=="?" ? x : (x-Lo[c])/(Hi[c] - Lo[c] + 1E-31) }

