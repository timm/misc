// bayes.js: incremental Naive Bayes classify (test-then-train)
// (c) 2026 Tim Menzies, timm@ieee.org, MIT license
//
// like + likes live in core.js (also used by acquire.js).

import { Data, clone, add, dinc, likes, keys } from "./core.js";

function classify(src, wait=10) {
  let it  = src[Symbol.iterator]();
  let all = Data([it.next().value]);
  let h={}, cf=null, n=0;
  for (let row of it) {
    let want = row[all.cols.klass.at];
    if (n >= wait) {
      let bk=null, bl=-Infinity, nk=keys(h).length;
      for (let kl in h) {
        let l = likes(h[kl], row, all.rows.length, nk);
        if (l > bl) { bl = l; bk = kl; } }
      cf = dinc(want, bk, cf); }
    if (!(want in h)) h[want] = clone(all);
    add(all, add(h[want], row));
    n++; }
  return cf; }

function confused(cf) {
  let klasses = new Set();
  for (let w in cf) { klasses.add(w); for (let g in cf[w]) klasses.add(g); }
  let total = 0;
  for (let w in cf) for (let g in cf[w]) total += cf[w][g];
  let p = (y, z) => Math.floor(100 * y / (z || 1e-32));
  let out = [];
  for (let c of [...klasses].sort()) {
    let tp = (cf[c] && cf[c][c]) || 0;
    let fn = 0; if (cf[c]) for (let g in cf[c]) fn += cf[c][g]; fn -= tp;
    let fp = 0; for (let w in cf) if (w !== c && cf[w][c]) fp += cf[w][c];
    let tn = total - tp - fn - fp;
    let pd = p(tp, tp+fn), pr = p(tp, fp+tp), sp = p(tn, tn+fp);
    out.push({
      tp, fn, fp, tn, pd, pr,
      f1:  Math.floor(2*pd*pr / (pd+pr + 1e-32)),
      g:   Math.floor(2*pd*sp / (pd+sp + 1e-32)),
      acc: p(tp+tn, total),
      label: "  " + c }); }
  return out; }

export { classify, confused };
