// kmeans.js: clustering via k-means, k-means++, and recursive halving
// (c) 2026 Tim Menzies, timm@ieee.org, MIT license

import {
  sample, choice, clone, distx, mids, pick, add, mn, flr
} from "./core.js";

let choices = (a, k) => {
  let out = [];
  for (let i = 0; i < k; i++) out.push(choice(a));
  return out; };

function kmeans(d, rs, k=10, n=10, cents) {
  rs    = rs    || d.rows;
  cents = cents || choices(rs, k);
  let out = [];
  for (let i = 0; i < n; i++) {
    out = cents.map(() => clone(d));
    for (let r of rs) {
      let bj=0, bd=Infinity;
      for (let j = 0; j < cents.length; j++) {
        let dd = distx(d, cents[j], r);
        if (dd < bd) { bd = dd; bj = j; } }
      add(out[bj], r); }
    cents = out.filter(kid => kid.rows.length).map(mids); }
  return out; }

function kpp(d, rs, k=10, few=256) {
  rs = rs || d.rows;
  let out = [choice(rs)];
  while (out.length < k) {
    let t = sample(rs, mn(few, rs.length));
    let ws = {};
    for (let i = 0; i < t.length; i++) {
      let bd = Infinity;
      for (let c of out) {
        let dd = distx(d, t[i], c) ** 2;
        if (dd < bd) bd = dd; }
      ws[i] = bd; }
    out.push(t[pick(ws)]); }
  return out; }

function half(d, rs, few=20) {
  let t = sample(rs, mn(few, rs.length));
  let gap=-1, east=null, west=null;
  for (let r1 of t) for (let r2 of t) {
    let g = distx(d, r1, r2);
    if (g > gap) { gap = g; east = r1; west = r2; } }
  let proj = r => (distx(d,r,east)**2 + gap**2 - distx(d,r,west)**2)
                / (2*gap + 1e-32);
  rs = rs.slice().sort((a,b) => proj(a) - proj(b));
  let n = flr(rs.length / 2);
  return [rs.slice(0,n), rs.slice(n), east, west, gap, proj(rs[n])]; }

function rhalf(d, rs, k=10, stop, few=20) {
  rs   = rs   || d.rows;
  stop = stop || 20;
  if (rs.length <= 2*stop) return [clone(d, rs)];
  let [l, r] = half(d, rs, few);
  return rhalf(d, l, k, stop, few).concat(rhalf(d, r, k, stop, few)); }

function neighbors(d, r1, ds, near=1, fast=false) {
  let bd=Infinity, bc=null;
  for (let c of ds) {
    let dd = distx(d, r1, mids(c));
    if (dd < bd) { bd = dd; bc = c; } }
  if (fast) return [mids(bc)];
  return bc.rows.slice()
               .sort((a,b) => distx(d,r1,a) - distx(d,r1,b))
               .slice(0, near); }

export { kmeans, kpp, half, rhalf, neighbors };
