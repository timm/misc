const fs  = require("fs");
const min = Math.min, max = Math.max, sqrt = Math.sqrt, abs = Math.abs
const log = Math.log, exp = Math.exp, PI = Math.PI;
const o   = (x, y = {}) => { for (let k in y) x[k] = y[k]; return x; };
const out = console.log

const the = { 
  Few:   50, 
  Stop:  32, 
  acq:   "xploit", 
  bins:  16, 
  boots: 512, 
  cliff: 0.147, 
  conf:  0.05, 
  file:  "data.csv", 
  guess: 0.5, 
  k:     1, 
  m:     2,
  Min:   4,
  p:     2, 
  rseed: 123456789, 
  start: 4 };

const Num = (txt = " ", at = 0) => o({
  it: "Num", txt: txt, at: at, n: 0, 
  mu: 0, sd: 0, m2: 0, lo: 1e30, hi: -1e30, rank: 0,
  goal: txt.at(-1) === "-" ? 0 : 1 });

const Sym = (txt = " ", at = 0) => o({
  it: "Sym", txt: txt, at: at, n: 0, 
  has: {} });


const Cols = names => names.reduce((cols, s, n) => {
  let col = /^[A-Z]/.test(s) ? Num(s, n) : Sym(s, n);
  cols.all.push(col);
  if (!s.endsWith("X")) {
    (/[!+-]/.test(s.at(-1)) ? cols.y : cols.x).push(col);
    if (s.endsWith("!")) cols.klass = col; }
  return cols;
}, o({ it: "Cols", names, all: [], x: [], y: [], klass: -1 }));

out({"adas","asdas",23})
out(Num("adssa",23))
