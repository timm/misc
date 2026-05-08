// acquire.js: active learning (warm start, then iterated best/rest scoring)
// (c) 2026 Tim Menzies, timm@ieee.org, MIT license
//
// likes lives in core.js (also used by bayes.js).

import {
  the, loadDoc, sqrt, flr, shuffle, clone, sub, add,
  distx, disty, mids, likes
} from "./core.js";

loadDoc(`
  --learn.start=4    initial labels
  --learn.budget=50  rows to evaluate
  --learn.check=5    guesses to check
`);

function acquireWithBayes(data, best, rest, row) {
  let n = best.rows.length + rest.rows.length;
  return likes(rest, row, n, 2) - likes(best, row, n, 2); }

let acquireWithCentroid = (data, best, rest, row) =>
  distx(data, row, mids(best)) - distx(data, row, mids(rest));

function warm_start(data, rows, label) {
  let lab = clone(data, rows.slice(0, the.learn.start));
  lab.rows.sort((a,b) => disty(lab, label(data,a)) - disty(lab, label(data,b)));
  let n = flr(sqrt(lab.rows.length));
  return [lab,
          clone(data, lab.rows.slice(0, n)),
          clone(data, lab.rows.slice(n)),
          rows.slice(the.learn.start)]; }

function dont_let_Best_grow_too_big(best, rest, lab) {
  if (best.rows.length > sqrt(lab.rows.length)) {
    best.rows.sort((a,b) => disty(lab, a) - disty(lab, b));
    let popped = best.rows.pop();
    rest.rows.push(add(rest.cols, sub(best.cols, popped))); } }

function acquire(data, score, label) {
  score = score || acquireWithCentroid;
  label = label || ((_, row) => row);
  let rows = shuffle(data.rows.slice());
  let [lab, best, rest, unlab] = warm_start(data, rows.slice(0, the.few), label);
  for (let i = 0; i < the.learn.budget; i++) {
    if (!unlab.length) break;
    unlab.sort((a,b) => score(lab,best,rest,a) - score(lab,best,rest,b));
    let pk = unlab.shift();
    add(lab, add(best, label(data, pk)));
    dont_let_Best_grow_too_big(best, rest, lab); }
  lab.rows.sort((a,b) => disty(lab, a) - disty(lab, b));
  return lab; }

export { acquireWithBayes, acquireWithCentroid, warm_start,
         dont_let_Best_grow_too_big, acquire };
