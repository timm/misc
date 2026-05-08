// trees.js: regression / decision trees over a Data
// (c) 2026 Tim Menzies, timm@ieee.org, MIT license

import {
  the, loadDoc, say, Num, add, adds, clone, mid, spread, disty, o, mn, flr
} from "./core.js";

loadDoc(`--learn.leaf=3   min examples per leaf`);

function treeCuts(col, rows) {
  let vs = [];
  for (let r of rows) if (r[col.at] !== "?") vs.push(r[col.at]);
  if (!vs.length) return [];
  if (col.type === "Sym") return [...new Set(vs)];
  vs.sort((a,b) => a-b);
  return [vs[flr(vs.length / 2)]]; }

function treeSplit(data, col, cut, rows) {
  let lr=[], rr=[], ln=Num(), rn=Num();
  for (let row of rows) {
    let v  = row[col.at];
    let go = v === "?" || (col.type === "Sym" ? v === cut : v <= cut);
    (go ? lr : rr).push(row);
    add(go ? ln : rn, disty(data, row)); }
  return {s: ln.n*spread(ln) + rn.n*spread(rn), col, cut, lr, rr}; }

function treeGrow(data, rows) {
  let tree = { type:"Tree", d: clone(data, rows),
               ynum: adds(rows.map(r => disty(data, r)), Num()),
               col: null, cut: 0, left: null, right: null };
  if (rows.length >= 2 * the.learn.leaf) {
    let valid = [];
    for (let col of tree.d.cols.xs)
      for (let cut of treeCuts(col, rows)) {
        let sp = treeSplit(data, col, cut, rows);
        if (mn(sp.lr.length, sp.rr.length) >= the.learn.leaf) valid.push(sp); }
    if (valid.length) {
      let best = valid.reduce((a,b) => b.s < a.s ? b : a);
      tree.col = best.col; tree.cut = best.cut;
      tree.left  = treeGrow(data, best.lr);
      tree.right = treeGrow(data, best.rr); } }
  return tree; }

function treeLeaf(tree, row) {
  if (!tree.left) return tree;
  let v  = row[tree.col.at];
  let go = v !== "?" && (tree.col.type === "Num" ? v <= tree.cut
                                                 : v === tree.cut);
  return treeLeaf(go ? tree.left : tree.right, row); }

function* treeNodes(tree, lvl=0, col=null, op="", cut=null) {
  yield [tree, lvl, col, op, cut];
  if (tree.col) {
    let ops  = tree.col.type === "Num" ? ["<=",">"] : ["==","!="];
    let kids = [[tree.left, ops[0]], [tree.right, ops[1]]];
    kids.sort((a,b) => mid(a[0].ynum) - mid(b[0].ynum));
    for (let [k, txt] of kids)
      if (k) yield* treeNodes(k, lvl+1, tree.col, txt, tree.cut); } }

function treeShow(tree) {
  for (let [t1, lvl, col, op, cut] of treeNodes(tree)) {
    let p = (lvl > 0 ? "|   ".repeat(lvl-1) : "")
          + (col ? `${col.txt} ${op} ${o(cut)}` : "");
    let g = Object.fromEntries(t1.d.cols.ys.map(c => [c.txt, mid(c)]));
    say(`${p.padEnd(the.show.show)},${o(mid(t1.ynum)).padStart(4)}`
      + ` ,(${String(t1.ynum.n).padStart(3)}), ${o(g)}`); } }

export { treeCuts, treeSplit, treeGrow, treeLeaf, treeNodes, treeShow };
