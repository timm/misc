# tree.awk -- decision/regression tree on dot.awk + numsym.awk + data.awk
# usage:
#   gawk -f <(prep<dot.awk) -f <(prep<numsym.awk) \
#        -f <(prep<data.awk) -f <(prep<tree.awk) config.csv data.csv
# emits one "pred,actual" line per test row -> pipe to metrics.awk

BEGIN { FS = " *, *" }

# --- file 1: config -> THE ------------------------------------------
FNR == NR { THE[$1] = $2 + 0; next }

# --- file 2 header --------------------------------------------------
FNR == 1 { D = new("data"); data_head(D, $0); next }

# --- file 2 rows ----------------------------------------------------
{
  if (FNR - 1 <= THE.wait) data_read(D, 1)
  else                     score_row(D, data_read(D, 0)) }

function score_row(d, r,    pred, actual) {
  if (!.d.tree) .d.tree = tree_train(d, .d.all, 0)
  pred   = tree_test(d, .d.tree, .d.rows[r])
  actual = y_eval(d, .d.rows[r])
  print pred "," actual }

END { rogues() }

# --- target evaluation: regression disty OR classification klass ----
function y_eval(d, row) {
  return (.d.ykind == "num") ? disty(d, row) : row[.d.klass] }

function disty(d, row,    i, c, s, n) {
  for (i in .d.y) {
    n++;  c = .d.cols[i]
    s += (num_norm(c, row[i]) - .d.y[i])^2 }
  return sqrt(s/n) }

# --- polymorphic y-column over row set ------------------------------
function ycol(d, rows,    y, r) {
  y = new(.d.ykind)
  for (r in rows) add(y, y_eval(d, .d.rows[rows[r]]), 1)
  return y }

function spread(d, rows,    y, v) {
  y = ycol(d, rows); v = var(y); free(y); return v }

function leaf_pred(d, rows,    y, v) {
  y = ycol(d, rows); v = mid(y); free(y); return v }

# --- splits ---------------------------------------------------------
function partition(d, rows, c, kids,    r, x, k, cut, key, cnt) {
  k   = .d.cols[c]
  cut = (.k.is == "num") ? mid(k) : ""
  for (r in rows) {
    x = .d.rows[rows[r]][c]
    if (x == "?") continue
    key = (.k.is == "num") ? (x < cut ? "lo" : "hi") : x
    kids[key][++cnt[key]] = rows[r] } }

function split_gain(d, rows, c,    kids, k, total, w, n) {
  partition(d, rows, c, kids)
  if (length(kids) < 2) return 0
  total = spread(d, rows)
  for (k in kids) {
    w += length(kids[k]) * spread(d, kids[k])
    n += length(kids[k]) }
  return total - w/n }

# --- tree -----------------------------------------------------------
function tree_train(d, rows, dep,    n, c, g, b, bc, k, kids, key) {
  n = new("node");  .n.mu = leaf_pred(d, rows)
  if (length(rows) <= THE.leaf || dep >= THE.maxd)
    { .n.kind = "leaf"; return n }
  for (c = 1; c <= .d.nc; c++)
    if (!(c in .d.y) && c != .d.klass &&
        (g = split_gain(d, rows, c)) > b) { b = g; bc = c }
  if (!b) { .n.kind = "leaf"; return n }
  k = .d.cols[bc];  partition(d, rows, bc, kids)
  .n.kind = "branch";  .n.col = bc
  if (.k.is == "num") .n.cut = mid(k)
  for (key in kids) .n.kids[key] = tree_train(d, kids[key], dep+1)
  return n }

function tree_test(d, n, row,    c, x, k, key) {
  if (.n.kind == "leaf") return .n.mu
  c = .n.col;  x = row[c]
  if (x == "?") return .n.mu
  k   = .d.cols[c]
  key = (.k.is == "num") ? (x < .n.cut ? "lo" : "hi") : x
  if (!(key in .n.kids)) return .n.mu
  return tree_test(d, .n.kids[key], row) }
