BEGIN { FS=","; N=32; SEED=1 }
      { gsub(/[ \t\r]*/,"")
        NR>1 ? data(NR-1) : meta() }
END   { report() }

function meta(     i) {
  srand(SEED)
  for(i=1;i<=NF;i++) {
    if ($i ~ /\+$/) w[i]=1
    if ($i ~ /-$/ ) w[i]=0 }
  for(i in w) hi[i] = - (lo[i]=1E32) }

function data(r,    i,v) {
  for(i in hi) {
    v = $i+0
    if ($i == v) $i = v
    if ($i > hi[i]) hi[i]=$i
    if ($i < lo[i]) lo[i]=$i }
  for(i=1; i<=NF; i++) rows[r][i]=$i  }

function report(  i,r,s) {
  for(r in rows) 
    if (rand() < N/NR) {
      s = ydist(rows[r])
      for(i=1; i<=NF; i++) s = s ", " rows[r][i];
      print s | "sort -n -t," }}

function ydist(r,    i,d) {
  for(i in w) d += abs(norm(lo[i],hi[i],r[i]) - w[i])^2;
  return (d/length(w)) ^ 0.5 }

function norm(lo,hi,x) { return (x-lo)/(hi-lo+1E-32) }
function abs(x)        { return x>=0 ? x : - x }
"""
data=Data(csv(the.files))
random.shuffle(data.rows)
best=sorted(data.rows[:the.Build], key=data.ydist)[0]
print(data.ydist(best),best)


Data(csv(the.files)) 
print(d.ydist(r := d.ydists(random.sample(d.rows, the.Build))[0]),b)


"""
