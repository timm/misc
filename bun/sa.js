// sa.js: simulated annealing via (1+1) search
// (c) 2026 Tim Menzies, timm@ieee.org, MIT license

import { rand, exp, mx, flr, picks, oneplus1 } from "./core.js";

function sa(d, oracle, restarts=0, m=0.5, budget=1000) {
  let n = mx(1, flr(m * d.cols.xs.length));
  let accept = (e, en, h, b) => en < e || rand() < exp((e-en) / (1 - h/b + 1e-32));
  function* mutate(s) { yield picks(d, s, n); }
  return oneplus1(d, mutate, accept, oracle, budget, restarts); }

export { sa };
