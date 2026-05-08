#!/usr/bin/env bun
// plan.js: counterfactual planner. Reads CSV, builds tree on train half,
//   finds tree leaf for the worst row, prints alternative leaves + the
//   x-column changes needed to land in each.
//
// Usage: bun plan.js <data.csv>

import {
  the, srand, Data, csv, clone, shuffle, mid, spread, disty, o, say, flr
} from "./core.js";
import { treeGrow, treeLeaf, treeNodes } from "./trees.js";

function* treePlan(tree, here) {
  let eps = the.stats.eps * spread(tree.ynum);
  for (let [there] of treeNodes(tree)) if (there.col === null) {
    let dy = mid(here.ynum) - mid(there.ynum);
    let diff = there.d.cols.xs
      .map((c,i) => [c, here.d.cols.xs[i]])
      .filter(([c,h]) => mid(c) !== mid(h))
      .map(([c]) => `${c.txt}=${o(mid(c))}`);
    if (dy > eps && diff.length) yield [dy, mid(there.ynum), diff]; } }

function ready(file) {
  let d = Data(csv(file));
  shuffle(d.rows);
  let h = flr(d.rows.length / 2);
  return [d,
          clone(d, d.rows.slice(0, h).slice(0, the.few)),
          d.rows.slice(h)]; }

let file = process.argv[2];
if (!file) { say("usage: bun plan.js <data.csv>"); process.exit(1); }

srand(the.seed);
let [d, d_train] = ready(file);
let tree  = treeGrow(d_train, d_train.rows);
let worst = d.rows.reduce((a,b) => disty(d,b) > disty(d,a) ? b : a);
let here  = treeLeaf(tree, worst);

say(`  now=${o(mid(here.ynum))}`);
for (let [dy, score, diff] of [...treePlan(tree, here)].sort((a,b) => a[0]-b[0]))
  say(`  ${o(score).padStart(6)} (dy=${o(dy)}) if ${diff.join(", ")}`);
