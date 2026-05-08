// ls.js: local search via (1+1) search (greedy accept, occasional bursts)
// (c) 2026 Tim Menzies, timm@ieee.org, MIT license

import { choice, rand, pick, oneplus1 } from "./core.js";

function ls(d, oracle, restarts=100, p=0.5, tries=20, budget=1000) {
  let accept = (e, en) => en < e;
  function* mutate(s) {
    let c     = choice(d.cols.xs);
    let times = rand() < p ? tries : 1;
    for (let i = 0; i < times; i++) {
      s = s.slice();
      s[c.at] = pick(c, s[c.at]);
      yield s; } }
  return oneplus1(d, mutate, accept, oracle, budget, restarts); }

export { ls };
