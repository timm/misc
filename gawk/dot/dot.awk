# dot.awk -- runtime for the .field / new / out conventions
# prep:  .f after value-char  ->  ["f"];   .x bare  ->  OBJ[x]

# --- runtime --------------------------------------------------------
function new(t,    it, fn) {
  it = ++NID;  .it.is = t
  fn = t "_init"
  return (fn in FUNCTAB) ? @fn(it) : it }

# out(i,x): scalar-return only. Zap HEAP past i, return scalar x.
# UNSAFE for aggregate survivors -- their cross-id refs become dangling.
function out(i, x, j) {
  for (j = i+1; j <= NID; j++) delete HEAP[j]
  return x }

# free(i): manual zap one slot. Use when caller knows what to drop.
function free(i) { delete HEAP[i] }

function arr(x) { x[""] = 0; delete x[""] }

# rogues(): warn on lowercase globals leaked at end of run.
function rogues(    i) {
  for (i in SYMTAB) if (i ~ /^[a-z]/) print "leak:", i > "/dev/stderr" }
