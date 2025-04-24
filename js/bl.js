const fs = require("fs");
const out = console.log
const min = Math.min, max = Math.max, sqrt = Math.sqrt, abs = Math.abs;
const log = Math.log, exp = Math.exp, PI = Math.PI;

function o(x, y = {}) { 
  for (let k in y) x[k] = y[k]; 
  return x}

const the = { 
  Few:   50, Stop:  32, acq: "xploit", bins: 16, boots: 512, 
  cliff: 0.147, conf: 0.05, file: "data.csv", guess: 0.5, 
  k:     1, m: 2, min: 4, p: 2, rseed: 123456789, start: 4 }

function Num(txt = " ", at = 0) {
  return {it: "Num", txt: txt, at: at, n: 0, 
          mu: 0, sd: 0, m2: 0, lo: 1e30, hi: -1e30, rank: 0,
          goal: txt.at(-1) === "-" ? 0 : 1 }}

function Sym(txt = " ", at = 0) {
  return {it: "Sym", txt: txt, at: at, n: 0, 
          has: {}}

function Cols(names) {
  return names.reduce((cols, s, n) => {
    let col = /^[A-Z]/.test(s) ? Num(s, n) : Sym(s, n);
    cols.all.push(col);
    if (!s.endsWith("X")) {
      (/[!+-]/.test(s.at(-1)) ? cols.y : cols.x).push(col);
      if (s.endsWith("!")) cols.klass = col; }
    return cols;
  }, { it: "Cols", names, all: [], x: [], y: [], klass: -1 })}

function Data(src = []) {
  return src.reduce((d, x) => (d.add(x), d),
   {it: "Data", n: 0, rows: [], cols: null })

Num.add = function(v, n = 1, f = 1) {
  if (v !== "?") {
    this.n  += f * n;
    this.lo  = min(v, this.lo);
    this.hi  = max(v, this.hi);
    if (f < 0 && this.n < 2)  
      this.mu = this.sd = 0
    else {
      let d    = v - this.mu;
      this.mu += f * d / this.n;
      this.m2 += f * d * (v - this.mu);
      this.sd  = this.n <= 2 ? 0 : sqrt(max(0, this.m2 / (this.n - 1)))}}
  return v; };

Sym.add = function(v, n = 1, f = 1) {
  if (v === "?") return v;
  this.n += f * n;
  this.has[v] = (this.has[v] || 0) + f * n;
  return v; };

Cols.add = function(row, n = 1, f = 1) {
  this.all.forEach(col => col.add(row[col.at], n, f));
  return row; };

Data.add = function(row, n = 1, f = 1) {
  if (!this.cols) this.cols = Cols(row);
  else this.rows.push(this.cols.add(row, n, f));
  return row; };
  
out(Cols(["adas","asdas","Age-"]).all)
out(Num(txt="Age-"))
