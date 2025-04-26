const fs = require("fs");
const out = console.log
const min = Math.min, max = Math.max, sqrt = Math.sqrt, abs = Math.abs
const log = Math.log, exp = Math.exp, PI = Math.PI
const isa = (x,a) => Object.assign(Object.create(x),a)
const O   = Object, entries=O.entries, values=O.values, keys=O.keys
 
the = { 
  Few:   50, Stop:  32, acq: "xploit", bins: 16, boots: 512, 
  cliff: 0.147, conf: 0.05, file: "data.csv", guess: 0.5, 
  k:     1, m: 2, min: 4, p: 2, rseed: 123456789, start: 4 }

//---------------------------------------------------------------------------
Num = {
  _(txt=" ", at=0) { return isa(Num, 
             { txt, at, n:0, mu:0, m2:0, lo:1e30, hi:-1e30, rank:0, 
               goal: txt.at(-1) === "-" ? 0 : 1})},             
  mid()      { return this.mu},
  sub(v,n=1) { return this.add(v,n=n,f=-1)},
  var()      { return this.n <= 2 ? 0 : sqrt(max(0,this.m2 / (this.n - 1)))}}

 Num.add = function(v, n=1, f=1) {
    if (v !== "?") {
      this.n += f * n
      this.lo = min(v, this.lo)
      this.hi = max(v, this.hi)
      if (f < 0 && this.n < 2) 
        this.mu = this.sd = 0
      else {
        let d = v - this.mu
        this.mu += f * d / this.n
        this.m2 += f * d * (v - this.mu)}}
    return v}
    
//---------------------------------------------------------------------------
Sym = {
  _(txt=" ", at=0) { return isa(Sym, 
             { txt, at, n:0, has:{}})}, 
  mid()      { return keys(this.has).reduce((a,b) => this.has[b] > this.has[a]?b:a)},
  sub(v,n=1) { return this.add(v,n=n,f=-1)},
  var()      { return -values(this.has).reduce((e,n) => e + (n/this.n)*log(n/this.n),0)}}

Sym.add = function(v, n=1, f=1) {
    if (v !== "?") {
      this.n += f * n
      this.has[v] = (this.has[v] || 0) + f * n} 
    return v}
    
//---------------------------------------------------------------------------
let a = Num._("age-");
let b = Num._("Mph+");
let c = Sym._("color");

a.add(10);
a.add(20);
b.add(400);
c.add("red");
c.add("blue");

out(a)
out(b)
out(c)
out(1)
out(c.mid())
out(c.var())
