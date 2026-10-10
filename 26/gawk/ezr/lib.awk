# lib.awk: shared by every stage of the ezr0 pipeline.
# Between stages, metadata rides on tagged csv lines:
#   =opt,Check,5
#   =col,<at>,<name>,<NUM|SYM>,<x|y>,<mu>,<sd>,<goal>
#   =best,<v1>,<v2>,...          (centroid, full row width)
#   =rest,<v1>,<v2>,...
# Everything untagged is a plain csv data row.
# A stage that sets PASS=1 forwards the metadata downstream.

$1 == "=opt"  { Opt[$2] = $3;  if (PASS) print; next }
$1 == "=best" { for (i=2; i<=NF; i++) Best[i-1] = $i
                if (PASS) print; next }
$1 == "=rest" { for (i=2; i<=NF; i++) Rest[i-1] = $i
                if (PASS) print; next }
$1 == "=col"  { Name[$2]=$3; Type[$2]=$4; Role[$2]=$5
                Mu[$2]=$6; Sd[$2]=$7
                if ($5 == "y") Goal[$2] = $8
                if (PASS) print; next }

function abs(x)          { return x < 0 ? -x : x }
function clamp(x,lo,hi)  { return x<lo ? lo : (x>hi ? hi : x) }

function norm(c,v,   z) {        # 0..1, by the logistic curve
  if (Type[c] == "SYM") return v
  z = clamp((v - Mu[c]) / (Sd[c] + 1e-32), -3, 3)
  return 1 / (1 + exp(-1.7 * z)) }

function gap(c,a,b) {            # two values of one column
  if (a == "?" || b == "?") return 1
  if (Type[c] == "SYM")     return a == b ? 0 : 1
  return abs(norm(c,a) - norm(c,b)) }

function ydist(row,   c,d,n) {   # how far from heaven
  for (c in Goal) { d += abs(norm(c,row[c]) - Goal[c])^2; n++ }
  return sqrt(d/n) }

function xdist(row,cen,   c,d,n) { # over the x columns
  for (c in Type) if (Role[c] == "x") {
    d += gap(c, row[c], cen[c])^2; n++ }
  return sqrt(d/n) }
