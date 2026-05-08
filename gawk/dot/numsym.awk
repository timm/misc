# numsym.awk -- NUM and SYM types + their polymorphic dispatch
# polymorphic ops:  add, like, var, mid     (also: num_norm)
# coercion happens inside *_add; here we only guard "?"

# --- dispatch (lives with num/sym, the types it switches over) -----
function add (k, x, train,  fn) { fn = .k.is "_add"
                                  return @fn(k, x, train) }
function like(k, x, p, m,   fn) { fn = .k.is "_like"
                                  return @fn(k, x, p, m) }
function var(it,    fn) { fn = .it.is "_var"; return @fn(it) }
function mid(it,    fn) { fn = .it.is "_mid"; return @fn(it) }

# --- NUM ------------------------------------------------------------
function num_add(it, x, train,    d, d2) {
  if (x == "?") return x
  x += 0
  if (!train) return x
  .it.n++
  d  = x - .it.mu;  .it.mu += d / .it.n
  d2 = x - .it.mu;  .it.m2 += d * d2
  return x }

function num_var(it) {
  return .it.n < 2 ? 0 : sqrt(.it.m2 / (.it.n - 1)) }

function num_mid(it) { return .it.mu }

function num_norm(it, x,    s, z) {
  if (x == "?") return 0.5
  s = num_var(it) + 1e-30
  z = (x - .it.mu) / s
  if (z < -3) z = -3; else if (z > 3) z = 3
  return 1 / (1 + exp(-1.7 * z)) }

function num_like(it, x, prior, m,    s, z) {
  if (x == "?") return 1
  s = num_var(it) + 1e-30;  z = (x - .it.mu) / s
  return exp(-z*z/2) / (s * 2.5066282746) }

# --- SYM ------------------------------------------------------------
function sym_init(it)   { arr(.it.has); return it }

function sym_add(it, x, train) {
  if (x == "?")  return x
  if (!train)    return x
  .it.n++; .it.has[x]++
  return x }

function sym_var(it,    e, k, p) {
  for (k in .it.has) { p = .it.has[k]/.it.n; e -= p*log(p) }
  return e }

function sym_mid(it,    k, b, bv) {
  bv = -1
  for (k in .it.has) if (.it.has[k] > bv) { bv = .it.has[k]; b = k }
  return b }

function sym_like(it, x, prior, m,    f) {
  if (x == "?") return 1
  f = (x in .it.has) ? .it.has[x] : 0
  return (f + m*prior) / (.it.n + m) }
