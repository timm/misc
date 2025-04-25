const fs = require("fs");
const out = console.log
const min = Math.min, max = Math.max, sqrt = Math.sqrt, abs = Math.abs;
const log = Math.log, exp = Math.exp, PI = Math.PI;

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
          has: {} }}

x = Num()
y = Num()
y.at=20
out(x)
out(y)
