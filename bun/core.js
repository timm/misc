#!/usr/bin/env bun
// core.js: shared structures, distance, stats, search core
// (c) 2026 Tim Menzies, timm@ieee.org, MIT license
//
// Plain objects with .type tag. No classes.
// Dispatch on it.type === "Num"|"Sym"|"Cols"|"Data".

import { readFileSync } from "node:fs";

// ---- Aliases --------------------------------------------------------
let say   = (...a) => console.log(...a),
    keys  = Object.keys,
    vals  = Object.values,
    ents  = Object.entries,
    isArr = Array.isArray,
    log   = Math.log,
    log2  = Math.log2,
    exp   = Math.exp,
    sqrt  = Math.sqrt,
    PI    = Math.PI,
    abs   = Math.abs,
    mn    = Math.min,
    mx    = Math.max,
    flr   = Math.floor,
    sum   = a => a.reduce((x,y) => x+y, 0);

// ---- Config ---------------------------------------------------------
let the = {};

function loadDoc(doc) {
  for (let m of doc.matchAll(/([\w.]+)=(\S+)/g))
    nest(the, m[1], thing(m[2])); }

// shared options (used in >1 app)
loadDoc(`
  --seed=1             random seed
  --p=2                Minkowski exponent
  --few=128            sample cap for cluster/acquire
  --show.show=30       tree display width
  --show.decimals=2    decimal places
  --stats.cliffs=0.195 Cliffs Delta threshold
  --stats.conf=1.36    KS test confidence
  --stats.eps=0.35     margin of error multiplier
  --bayes.m=2          m-estimate for NB
  --bayes.k=1          k-estimate (Laplace) for NB
`);

// ---- RNG (mulberry32) -----------------------------------------------
let _seed = 1;
function srand(s) { _seed = (s>>>0) || 1; }
function rand() {
  _seed = (_seed + 0x6D2B79F5) | 0;
  let t = _seed;
  t = Math.imul(t ^ (t>>>15), t | 1);
  t ^= t + Math.imul(t ^ (t>>>7), t | 61);
  return ((t ^ (t>>>14)) >>> 0) / 4294967296; }

let choice = a => a[flr(rand() * a.length)];

function shuffle(a) {
  for (let i = a.length-1; i > 0; i--) {
    let j = flr(rand() * (i+1));
    [a[i], a[j]] = [a[j], a[i]]; }
  return a; }

let sample = (a, n) => shuffle(a.slice()).slice(0, n);

// ---- Columns: Num / Sym ---------------------------------------------
let Col = (txt="", a=0) => /^[A-Z]/.test(txt) ? Num(txt,a) : Sym(txt,a);

let Num = (txt="", a=0) => ({type:"Num", txt, at:a, n:0, mu:0, m2:0, sd:0,
                                    heaven: txt.slice(-1) !== "-"});
let Sym = (txt="", a=0) => ({type:"Sym", txt, at:a, n:0, has:{}});

function mode(d) {
  let bk=null, bn=-Infinity;
  for (let k in d) if (d[k] > bn) { bn = d[k]; bk = k; }
  return bk; }

function entropy(d) {
  let n = sum(vals(d));
  return -sum(vals(d).map(v => v/n * log2(v/n))); }

let mid    = c => c.type === "Num" ? c.mu : mode(c.has);
let spread = c => c.type === "Num" ? c.sd : entropy(c.has);

function norm(num, v) {
  if (v === "?") return v;
  let z = mx(-3, mn(3, (v - num.mu) / (num.sd + 1e-32)));
  return 1 / (1 + exp(-1.7*z)); }

// ---- Cols & Data ----------------------------------------------------
function Cols(names) {
  let c = {type:"Cols", names, klass:null, xs:[], ys:[], all:[]};
  names.forEach((txt, j) => {
    let col = Col(txt, j);
    c.all.push(col);
    if (txt.slice(-1) !== "X") {
      if (txt.slice(-1) === "!") c.klass = col;
      ("+-!".includes(txt.slice(-1)) ? c.ys : c.xs).push(col); } });
  return c; }

function Data(src) {
  let it = (src || [])[Symbol.iterator]();
  let head = it.next();
  if (head.done) throw new Error("Data needs header row");
  let d = {type:"Data", rows:[], _centroid:null, cols: Cols(head.value)};
  for (let row of it) add(d, row);
  return d; }

function clone(data, rows) {
  let d = Data([data.cols.names]);
  for (let r of (rows || [])) add(d, r);
  return d; }

let sub = (it, v) => add(it, v, -1);

function add(it, v, w=1) {
  if (it.type === "Data") {
    it._centroid = null;
    add(it.cols, v, w);
    if (w > 0) it.rows.push(v);
    else { let i = it.rows.indexOf(v); if (i >= 0) it.rows.splice(i, 1); } }
  else if (it.type === "Cols") {
    for (let col of it.all) add(col, v[col.at], w); }
  else if (v !== "?") {
    if (it.type === "Sym") it.has[v] = w + (it.has[v] || 0);
    else if (w < 0 && it.n <= 2) { it.n = it.mu = it.m2 = it.sd = 0; }
    else {
      it.n  += w;
      let delta = v - it.mu;
      it.mu += w * delta / it.n;
      it.m2 += w * delta * (v - it.mu);
      it.sd  = it.n > 1 ? sqrt(mx(0, it.m2) / (it.n - 1)) : 0; } }
  return v; }

let mids = data =>
  data._centroid = data._centroid || data.cols.all.map(mid);

function adds(src, it) {
  it = it || Num();
  for (let v of (src || [])) add(it, v);
  return it; }

// ---- Distance -------------------------------------------------------
function minkowski(items, p=2) {
  let tot=0, n=1e-32;
  for (let v of items) { tot += v**p; n += 1; }
  return (tot/n) ** (1/p); }

let disty = (data, row) => minkowski(
  data.cols.ys.map(y => abs(norm(y, row[y.at]) - (y.heaven ? 1 : 0))), the.p);

let distx = (data, r1, r2) => minkowski(
  data.cols.xs.map(x => aha(x, r1[x.at], r2[x.at])), the.p);

function aha(col, u, v) {
  if (u === "?" && v === "?") return 1;
  if (col.type === "Sym") return u !== v ? 1 : 0;
  let nu = norm(col, u), nv = norm(col, v);
  if (nu === "?") nu = nv > 0.5 ? 0 : 1;
  if (nv === "?") nv = nu > 0.5 ? 0 : 1;
  return abs(nu - nv); }

function nearest(data, row, rows) {
  rows = rows || data.rows;
  let best=null, bd=Infinity;
  for (let r of rows) { let d = distx(data, row, r);
                        if (d < bd) { bd = d; best = r; } }
  return best; }

// ---- Scoring (win) --------------------------------------------------
function wins(data) {
  let ys  = data.rows.map(r => disty(data, r)).sort((a,b) => a-b);
  let ten = flr(ys.length / 10);
  let lo  = ys[0], med = ys[5*ten];
  let sd  = (ys[9*ten] - ys[ten]) / 2.56;
  return row => {
    let x = disty(data, row);
    if (x < lo + 0.35*sd) x = lo;
    return mx(-100, flr(100 * (1 - (x-lo) / (med-lo + 1e-32)))); }; }

// ---- Bayes likelihoods (likes used by bayes + acquire) --------------
function like(col, v, prior) {
  if (col.type === "Sym")
    return ((col.has[v] || 0) + the.bayes.k * prior) / (col.n + the.bayes.k);
  let sd = col.sd + 1e-32, z = 2*sd*sd;
  return exp(-((v - col.mu)**2) / z) / sqrt(PI * z); }

function likes(data, row, n_rows, n_klasses) {
  let prior = (data.rows.length + the.bayes.m) /
              (n_rows + the.bayes.m * n_klasses);
  let s = log(prior);
  for (let col of data.cols.xs) {
    let v = row[col.at];
    if (v === "?") continue;
    let x = like(col, v, prior);
    if (x > 0) s += log(x); }
  return s; }

// ---- Sampling / mutation -------------------------------------------
function pick(it, v) {
  if (it && it.type === "Sym") return pick(it.has);
  if (it && it.type === "Num") {
    let tmp  = (v !== undefined && v !== "?") ? v : it.mu;
    let lo   = it.mu - 3*it.sd, hi = it.mu + 3*it.sd;
    let newv = tmp + it.sd * 2 * (rand() + rand() + rand() - 1.5);
    return lo + ((newv - lo) % (hi - lo + 1e-32)); }
  let n = sum(vals(it)) * rand();
  let last;
  for (let k in it) { last = k; n -= it[k]; if (n <= 0) return k; }
  return last; }

function picks(data, row, n=1) {
  let s = row.slice();
  for (let col of sample(data.cols.xs, mn(n, data.cols.xs.length)))
    s[col.at] = pick(col, s[col.at]);
  return s; }

// ---- (1+1) search core (used by sa + ls) ----------------------------
function* oneplus1(data, mutate, accept, oracle, budget=1000, restart=0) {
  let h=0, best=null, best_e=1e32;
  let s=choice(data.rows).slice(), e=1e32, imp=0;
  while (h < budget) {
    for (let sn of mutate(s)) {
      h++;
      let en = oracle(sn);
      if (accept(e, en, h, budget)) { s = sn; e = en; }
      if (en < best_e) { best = sn.slice(); best_e = en; imp = h;
                         yield [h, best_e, best]; }
      if (restart && h - imp > restart) {
        s = choice(data.rows).slice();
        e = 1e32; imp = h;
        break; } } } }

function oracleNearest(data, row) {
  let near = nearest(data, row);
  for (let col of data.cols.ys) row[col.at] = near[col.at];
  return disty(data, row); }

function last(gen) {
  let v = null;
  for (let x of gen) v = x;
  return v; }

// ---- Stats ----------------------------------------------------------
function bisectLeft(a, x) {
  let lo=0, hi=a.length;
  while (lo < hi) { let m = (lo+hi) >> 1; if (a[m] < x) lo = m+1; else hi = m; }
  return lo; }

function bisectRight(a, x) {
  let lo=0, hi=a.length;
  while (lo < hi) { let m = (lo+hi) >> 1; if (x < a[m]) hi = m; else lo = m+1; }
  return lo; }

function same(xs, ys, eps) {
  xs = xs.slice().sort((a,b) => a-b);
  ys = ys.slice().sort((a,b) => a-b);
  let n=xs.length, m=ys.length;
  if (abs(xs[flr(n/2)] - ys[flr(m/2)]) <= eps) return true;
  let gt=0, lt=0;
  for (let a of xs) { gt += bisectLeft(ys, a); lt += m - bisectRight(ys, a); }
  if (abs(gt-lt) / (n*m) > the.stats.cliffs) return false;
  let ks = v => abs(bisectRight(xs,v)/n - bisectRight(ys,v)/m);
  let kmax = 0;
  for (let v of xs) kmax = mx(kmax, ks(v));
  for (let v of ys) kmax = mx(kmax, ks(v));
  return kmax <= the.stats.conf * sqrt((n+m) / (n*m)); }

function bestRanks(d) {
  let items = ents(d).map(([k,v]) => {
    let sv = v.slice().sort((a,b) => a-b);
    return [k, v, sv[flr(sv.length/2)]]; });
  items.sort((a,b) => a[2] - b[2]);
  let [k0, lst0] = items[0];
  let best = {[k0]: adds(lst0, Num(k0))};
  for (let i = 1; i < items.length; i++) {
    let [k, lst] = items[i];
    if (same(lst0, lst, spread(best[k0]) * the.stats.eps))
      best[k] = adds(lst, Num(k));
    else break; }
  return best; }

// ---- Utilities ------------------------------------------------------
function dinc(k1, k2, b4) {
  b4 = b4 || {};
  b4[k1] = b4[k1] || {};
  b4[k1][k2] = (b4[k1][k2] || 0) + 1;
  return b4; }

function o(x) {
  if (typeof x === "number" && !Number.isInteger(x))
    return x.toFixed(the.show.decimals);
  if (isArr(x)) return "{" + x.map(o).join(", ") + "}";
  if (x && typeof x === "object") {
    if (x.type) { let d = {...x}; delete d.type; return x.type + o(d); }
    let ks = keys(x).sort();
    return "{" + ks.map(k => `${k}=${o(x[k])}`).join(", ") + "}"; }
  return String(x); }

function table(lst, w=10) {
  if (!lst.length) return;
  let ks = keys(lst[0]);
  say(ks.map(k => String(k).padStart(w)).join(""));
  say("-".repeat(ks.length * w));
  for (let d of lst) say(ks.map(k => String(d[k] ?? "").padStart(w)).join("")); }

function thing(txt) {
  txt = String(txt).trim();
  if (txt === "true")  return true;
  if (txt === "false") return false;
  if (/^-?\d+$/.test(txt)) return parseInt(txt, 10);
  if (/^-?\d*\.\d+([eE]-?\d+)?$/.test(txt)) return parseFloat(txt);
  if (/^-?\d+[eE]-?\d+$/.test(txt))         return parseFloat(txt);
  return txt; }

function nest(t, k, v) {
  let ks = k.split(".");
  for (let i = 0; i < ks.length-1; i++) {
    if (!t[ks[i]]) t[ks[i]] = {};
    t = t[ks[i]]; }
  t[ks[ks.length-1]] = v; }

function* csv(path, clean) {
  clean = clean || (txt => txt.split("#")[0].split(","));
  let text = readFileSync(path, "utf-8");
  for (let line of text.split("\n")) {
    let row = clean(line);
    if (row.some(x => String(x).trim())) yield row.map(thing); } }

// ---- Boot -----------------------------------------------------------
srand(the.seed);

// ---- Exports --------------------------------------------------------
export {
  say, keys, vals, ents, isArr, log, log2, exp, sqrt, PI, abs, mn, mx, flr, sum,
  the, loadDoc,
  srand, rand, choice, shuffle, sample,
  Col, Num, Sym, Cols, Data, clone, sub, add, mids, adds,
  mid, spread, mode, entropy, norm,
  minkowski, disty, distx, aha, nearest, wins,
  like, likes,
  pick, picks,
  oneplus1, oracleNearest, last,
  bisectLeft, bisectRight, same, bestRanks,
  dinc, o, table, thing, nest, csv };
