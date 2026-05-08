# bayes.awk -- Naive Bayes on dot.awk + numsym.awk + data.awk
# usage:
#   gawk -f <(prep<dot.awk) -f <(prep<numsym.awk) \
#        -f <(prep<data.awk) -f <(prep<bayes.awk) config.csv data.csv
# emits one "pred,actual" line per test row -> pipe to metrics.awk

BEGIN { FS = " *, *" }

FNR == NR { THE[$1] = $2 + 0; next }

FNR == 1 { D = new("data"); data_head(D, $0); next }

{
  if (FNR - 1 <= THE.wait) train_row()
  else                     test_row() }

END { rogues() }

function train_row(    r, lbl, i) {
  r = data_read(D, 1)
  lbl = .D.rows[r][.D.klass]
  N[lbl]++
  if (!(lbl in HAVE)) {
    HAVE[lbl] = 1
    for (i = 1; i <= .D.nc; i++)
      if (i != .D.klass) COL[lbl][i] = new(.D.nump[i] ? "num" : "sym") }
  for (i = 1; i <= .D.nc; i++)
    if (i != .D.klass) add(COL[lbl][i], .D.rows[r][i], 1) }

function test_row(    r, pred, actual) {
  r = data_read(D, 0)
  pred   = bayes_pred(.D.rows[r])
  actual = .D.rows[r][.D.klass]
  print pred "," actual }

function bayes_pred(row,    lbl, i, ll, best, blbl, total, prior, nc, first) {
  for (lbl in N) { total += N[lbl]; nc++ }
  first = 1
  for (lbl in N) {
    prior = (N[lbl] + THE.k) / (total + THE.k * nc)
    ll = log(prior)
    for (i = 1; i <= .D.nc; i++) {
      if (i == .D.klass) continue
      ll += log(safe_like(COL[lbl][i], row[i], prior)) }
    if (first || ll > best) { best = ll; blbl = lbl; first = 0 } }
  return blbl }

function safe_like(c, x, prior,    v) {
  v = like(c, x, prior, THE.m)
  return v > 1e-30 ? v : 1e-30 }
