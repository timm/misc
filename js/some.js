const fs = require("fs");
const out = console.log
const min = Math.min, max = Math.max, sqrt = Math.sqrt, abs = Math.abs
const log = Math.log, exp = Math.exp, PI = Math.PI
const O   = Object, entries = O.entries, assign = O.assign
const create = O.create, values = O.values

const the = { 
  Few:   50, Stop:  32, acq: "xploit", bins: 16, boots: 512, 
  cliff: 0.147, conf: 0.05, file: "data.csv", guess: 0.5, 
  k:     1, m: 2, min: 4, p: 2, rseed: 123456789, start: 4 }

//---------------------------------------------------------------------------
const Num = {
  make: (txt=" ", at=0) => assign(create(Num), 
             {txt, at, n:0, mu:0, m2:0, lo:1e30, hi:-1e30, rank:0, 
              goal: txt.at(-1) === "-" ? 0 : 1}),             
  add(v, n=1, f=1) {
    if (v !== "?") {
      this.n += f * n;
      this.lo = min(v, this.lo);
      this.hi = max(v, this.hi);
      if (f < 0 && this.n < 2) 
        this.mu = this.sd = 0
      else {
        let d = v - this.mu;
        this.mu += f * d / this.n;
        this.m2 += f * d * (v - this.mu)}}
    return v},
    
  mid()      { return this.mu; },
  sub(v,n=1) { return this.add(v,n=n,f=-1) },
  var()      { return this.n <= 2 ? 0 : sqrt(max(0,this.m2 / (this.n - 1)))}}

//---------------------------------------------------------------------------
const Sym = {
  make: (txt=" ", at=0) => assign(create(Sym), {txt, at, n:0, has:{}}),
  add(v, n=1, f=1) {
    if (v !== "?") {
      this.n += f * n;
      this.has[v] = (this.has[v] || 0) + f * n} 
    return v},
    
  mid()      { return entries(this.has)
                      .reduce((a,b) => a[1]>b[1] ? a : b, ["?",0])[0] },
  sub(v,n=1) { return this.add(v,n=n,f=-1) },
  var()      { return -values(this.has)
                      .reduce((e,n) => e + (n/this.n)*log(n/this.n), 0) }}

//---------------------------------------------------------------------------
let a = Num.make("age-");
let b = Num.make("Mph+");
let c = Sym.make("color");

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
out(2)
