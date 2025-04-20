Recreate a minimalist JavaScript data engine in the "Bare Style" format:

No classes or prototypes

Use factory functions (Num, Sym, Cols, Data)

Define .add methods separately after objects are created

Include helpers: csv(), coerce(), adds(), clone(), seeded()

Support test functions in an eg object, callable via command line

Add a test runner: if (require.main === module) block

Ensure it's side-effect-free (no file writes)

Add module.exports = { ... } if used as a module

Keep code very tight and Lua-like — no clutter

For any line containing just a closing braclet "}", join it to the previous line

// Bare Style — A minimal JavaScript engine for explainable, data-driven learning
// All code here was compiled from a ChatGPT-assisted port of bl.py
// Includes: Num, Sym, Data, ydist, actLearn, tree, bootstrap, cliffs, and CLI test runner

const fs = require("fs");
const min = Math.min, max = Math.max, sqrt = Math.sqrt, abs = Math.abs, log = Math.log, exp = Math.exp, PI = Math.PI;
const o = (x, y = {}) => { for (let k in y) x[k] = y[k]; return x; };

const the = { file: "data.csv", rseed: 123456789, acq: "xploit", p: 2, guess: 0.5, Stop: 32, start: 4, Few: 50, m: 2, k: 1, cliff: 0.147, boots: 512, conf: 0.05, bins: 16, min: 4 };

const Num = (of = " ", at = 0) => o({
  it: "Num", of, at, n: 0, mu: 0, sd: 0, m2: 0, lo: 1e30, hi: -1e30, rank: 0,
  goal: of.at(-1) === "-" ? 0 : 1 });

const Sym = (of = " ", at = 0) => o({
  it: "Sym", of, at, n: 0, has: {} });

const Cols = names => names.reduce((cols, s, n) => {
  let col = /^[A-Z]/.test(s) ? Num(s, n) : Sym(s, n);
  cols.all.push(col);
  if (!s.endsWith("X")) {
    (/[!+-]/.test(s.at(-1)) ? cols.y : cols.x).push(col);
    if (s.endsWith("!")) cols.klass = col;
  }
  return cols;
}, o({ it: "Cols", names, all: [], x: [], y: [], klass: -1 }));

const Data = (src = []) => src.reduce((d, x) => (d.add(x), d),
  o({ it: "Data", n: 0, rows: [], cols: null }));

Num.add = function (v, n = 1, f = 1) {
  if (v === "?") return v;
  this.n += f * n;
  this.lo = min(v, this.lo); this.hi = max(v, this.hi);
  if (f < 0 && this.n < 2) return this.mu = this.sd = 0;
  let d = v - this.mu;
  this.mu += f * d / this.n;
  this.m2 += f * d * (v - this.mu);
  this.sd = this.n <= 2 ? 0 : sqrt(max(0, this.m2 / (this.n - 1)));
  return v; };

Sym.add = function (v, n = 1, f = 1) {
  if (v === "?") return v;
  this.n += f * n;
  this.has[v] = (this.has[v] || 0) + f * n;
  return v; };

Cols.add = function (row, n = 1, f = 1) {
  this.all.forEach(col => col.add(row[col.at], n, f));
  return row; };

Data.add = function (row, n = 1, f = 1) {
  if (!this.cols) this.cols = Cols(row);
  else this.rows.push(this.cols.add(row, n, f));
  return row; };

const isNum = x => typeof x === "number";
const first = x => x[0];
const rand = Math.random;
const one = (xs) => xs[Math.floor(rand() * xs.length)];
const some = (xs, k = 1) => Array.from({ length: k }, () => one(xs));
const BIG = 1e32;

const coerce = s => {
  s = s.trim();
  if (s === "True") return true;
  if (s === "False") return false;
  let n = +s;
  return isNaN(n) ? s : n; };

const csv = file => fs.readFileSync(file === "-" ? 0 : file, "utf-8")
  .split(/\r?\n/)
  .map(x => x.replace(/[\t\r ]|#.*$/g, ""))
  .filter(x => x)
  .map(x => x.split(",").map(coerce));

const adds = (rows, i) => rows.reduce((r, x) => (r ??= isNum(x) ? Num() : Sym(), r.add(x), r), i);
const sub = (v, i, n = 1) => i.add(v, n, -1);
const clone = (data, rows = []) => adds(rows, Data([data.cols.names]));

const norm = (v, col) => {
  (v === "?" || col.it === "Sym") ? v : (v - col.lo) / (col.hi - col.lo + 1 / BIG) }

const mid = col => {
  col.it === "Num" ? col.mu : Object.entries(col.has).reduce((a, b) => a[1] > b[1] ? a : b)[0] }

const spread = c => {
  c.it === "Num" ? c.sd : -Object.values(c.has).reduce((acc, n) => acc + (n / c.n) * log(n / c.n), 0)}

const ydist = (row, data) => {
  let sumPowers = data.cols.y.reduce((acc, c) => acc + abs(norm(row[c.at], c) - c.goal) ** the.p, 0);
  return (sumPowers / data.cols.y.length) ** (1 / the.p); };

const yNums = (rows, data) => adds(rows.map(row => ydist(row, data)));

const like = (row, data, nall = 100, nh = 2) => {
  const prior = (data.n + the.k) / (nall + the.k * nh);
  const _col = (v, col) => {
    if (col.it === "Sym") return (col.has[v] || 0 + the.m * prior) / (col.n + the.m + 1 / BIG);
    let sd = col.sd + 1 / BIG;
    let nom = exp(-1 * (v - col.mu) ** 2 / (2 * sd * sd));
    let denom = sqrt(2 * PI * sd * sd);
    return max(0, min(1, nom / denom));
  };
  let probs = data.cols.x.map(col => {
    let v = row[col.at];
    return v === "?" ? 1 : _col(v, col);
  });
  return probs.concat([prior]).filter(n => n > 0).reduce((a, b) => a + log(b), 0); };

const actLearn = (data, shuffle = true) => {
  const _acquire = (p, b, r) => {
    b = exp(b); r = exp(r);
    let q = the.acq === "xploit" ? 0 : (the.acq === "xplore" ? 1 : 1 - p);
    return (b + r * q) / abs(b * q - r + 1 / BIG);
  };
  const _guess = row => _acquire(n / the.Stop, like(row, best, n, 2), like(row, rest, n, 2));

  if (shuffle) data.rows = [...data.rows].sort(() => rand() - 0.5);
  let n = the.start;
  let todo = data.rows.slice(n);
  let done = [...data.rows.slice(0, n)].sort((a, b) => ydist(a, data) - ydist(b, data));
  let cut = Math.round(n ** the.guess);
  let best = clone(data, done.slice(0, cut));
  let rest = clone(data, done.slice(cut));

  while (todo.length > 2 && n < the.Stop) {
    n++;
    let candidates = todo.slice(0, the.Few * 2).sort((a, b) => _guess(b) - _guess(a));
    let hi = candidates[0], lo = candidates.slice(1, the.Few + 1);
    todo = candidates.slice(the.Few * 2).concat(lo);
    add(hi, best);
    best.rows = best.rows.sort((a, b) => ydist(a, data) - ydist(b, data));
    if (best.rows.length >= Math.round(n ** the.guess)) {
      let popped = best.rows.pop();
      add(sub(popped, best), rest);
    }
  }
  return o({ best, rest }); };

// Compute Cliff's Delta effect size between two lists
const cliffs = (lst1, lst2) => {
  let n = 0, gt = 0;
  for (let x of lst1)
    for (let y of lst2) {
      n++;
      if (x > y) gt++;
      else if (x < y) gt--;
    }
  return abs(gt / n) > the.cliff;
};

// Perform bootstrap hypothesis test between two samples
const bootstrap = (y, z) => {
  let x = y.concat(z);
  let ymu = mid(adds(y)), zmu = mid(adds(z));
  let tobs = abs(ymu - zmu);
  let bigger = 0;
  for (let i = 0; i < the.boots; i++) {
    let ys = some(x, y.length);
    let zs = some(x, z.length);
    let yhat = mid(adds(ys)), zhat = mid(adds(zs));
    if (abs(yhat - zhat) > tobs) bigger++;
  }
  return bigger / the.boots < the.conf;
};

// Generate cut points for discretizing numeric columns
const cuts = (col, bin = the.bins) => {
  let step = Math.floor(col.n / bin);
  return col.has ? Object.keys(col.has) :
    col.n < 2 * step ? [col.lo, col.hi] :
    Array.from({ length: bin }, (_, i) => col.lo + (col.hi - col.lo) * i / bin);
};

// Recursive decision tree based on best splits of numeric columns
const tree = (data, tiny = the.min) => {
  const recurse = rows => {
    if (rows.length <= tiny) return rows;
    let best = 0, cut = null;
    for (let col of data.cols.x) {
      let bins = cuts(col);
      for (let i = 1; i < bins.length; i++) {
        let left = [], right = [];
        for (let row of rows) {
          let v = row[col.at];
          if (v !== "?") (v <= bins[i - 1] ? left : right).push(row);
        }
        let delta = abs(mid(yNums(left, data)) - mid(yNums(right, data)));
        if (delta > best) best = delta, cut = [col, bins[i - 1], left, right];
      }
    }
    return cut ? [cut[0].at, cut[1], recurse(cut[2]), recurse(cut[3])] : rows;
  };
  return recurse(data.rows);
};


/ --- Test Hooks ---
const eg = {};

eg.csv = file => {
  let rows = csv(file || the.file);
  if (rows.length !== 3192) throw "bad row count";
  if (typeof rows[1][0] !== "number") throw "expected number in first column";
};

// --- Seeded RNG helper ---
const seeded = seed => () => (seed = (seed * 9301 + 49297) % 233280) / 233280;

// --- CLI test runner ---
if (require.main === module) {
  let [,, ...args] = process.argv;
  let fun = eg[(args[0] || "").replace("-", "_")];
  if (!fun) throw "unknown test: " + args[0];
  Math.random = seeded(the.rseed); // use global seeded RNG
  fun(coerce(args[1] || ""));
}
