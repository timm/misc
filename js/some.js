const help=`
bl.js : barelogic, XAI for active learning + multi-objective optimization
(c) 2025, Tim Menzies <timm@ieee.org>, MIT License  

OPTIONS:  

      -a acq        xploit or xplore or adapt   = xploit  
      -b bootstraps num of bootstrap samples    = 512
      -B BootConf   bootstrap threshold         = 0.95
      -B BootConf   bootstrap threshold         = 0.95
      -c cliffConf  cliffs delta threshold     = 0.197
      -C Cohen      Cohen threshold             = 0.35
      -d decs       decimal places for printing = 3  
      -f file       training csv file           = auto93.csv  
      -F Few        search a few items in a list = 50
      -g guess      size of guess               = 0.5  
      -k k          low frequency Bayes hack    = 1  
      -l leaf       min size of tree leaves     = 2
      -m m          low frequency Bayes hack    = 2  
      -p p          distance formula exponent   = 2  
      -r rseed      random number seed          = 1234567891  
      -s start      where to begin              = 4  
      -S Stop       where to end                = 32  
      -t tiny       min size of leaves of tree  = 4
`
fs = require("fs");
out = console.log
min = Math.min, max = Math.max, sqrt = Math.sqrt, abs = Math.abs
round = Math.round
log = Math.log, exp = Math.exp, PI = Math.PI
isa = (x,a) => Object.assign(Object.create(x),a)
assert = (test,txt) => {if (!test) throw new Error(txt)}
//---------------------------------------------------------------------------------------
Num = {
  _(txt=" ", at=0) { return isa(Num, 
             { txt, at, n:0, mu:0, m2:0, lo:1e30, hi:-1e30, rank:0, 
               goal: txt.at(-1) === "-" ? 0 : 1})},             
  mid()      { return this.mu},
  sub(v,n=1) { return this.add(v,n=n,f = -1)},
  var()      { return this.n <= 2 ? 0 : sqrt(max(0,this.m2 / (this.n - 1)))}}

Num.add = function(v, n=1, f=1) {
    if (v !== "?") {
      this.n += f * n
      this.lo = min(v, this.lo)
      this.hi = max(v, this.hi)
      if (f < 0 && this.n < 2) {
        this.mu = this.sd = 0
      } else {
        let d = v - this.mu
        this.mu += f * d / this.n
        this.m2 += f * d * (v - this.mu)}}
    return v}
    
//---------------------------------------------------------------------------------------
Sym = {
  _(txt=" ", at=0) { return isa(Sym, { txt, at, n:0, has:{}})}, 
  mid()      { return mode(this.has) },
  sub(v,n=1) { return this.add(v,n=n,f = -1) },
  var()      { return entropy(this.has) } }

Sym.add = function(v, n=1, f=1) {
    if (v !== "?") {
      this.n += f * n
      this.has[v] = (this.has[v] || 0) + f * n} 
    return v}

//---------------------------------------------------------------------------------------
function Cols(names) { 
  return names.reduce((cols, s, n) => {
    let col = (/^[A-Z]/.test(s) ? Num : Sym)._(s, n)
    cols.all.push(col);
    if (!s.endsWith("X")) {
      (/[!+-]/.test(s.at(-1)) ? cols.y : cols.x).push(col);
      if (s.endsWith("!")) cols.klass = col; }
    return cols;
  }, isa(Cols, {names, all: [], x: [], y: [], klass: -1 })) }

 Cols.add = a => this.cols.all.forEach(col => col.add(a[col.at]))
 
//--------------------------------------------------------------------------------------
function mode(obj) {
  return Object.keys(obj).reduce((a,b) => obj[b] > obj[a]?b:a)}

function entropy(obj) {
  let m=0; for (let k in obj) { m += obj[k] };
  let e=0; for (let k in obj) { e -= obj[k]/m*log(obj[k]/m,2) }; return e }
  
function cli(obj = {}, eg = {}, args = process.argv.slice(2)) {
  for (let i = 0, a; i < args.length; i++)
    if ((a = args[i])[0] === "-")
      if (a[1] === "-") {
        let f = eg[a.slice(2)];
        if (f) {
          SEED = the.rseed;  // <-- use RNG here
          f((!args[i+1] || args[i+1][0] === "-") ? undefined : is(args[++i]))}
      } else
          for (let k in obj)
            if (a[1] === k[0])
              obj[k] = typeof obj[k] === "boolean" ? !obj[k] : is(args[++i]);
    return obj }

function is(x) {
  if (x === "?") return "?"
  if (x === "true") return true
  if (x === "false") return false
  let y = +x;
  return isNaN(y) ? x : y }

function csv(file) { 
  return fs.readFileSync(file === "-" ? 0 : file, "utf-8")
    .split(/\r?\n/).map(x => x.replace(/[\t\r ]|#.*$/g, ""))
    .filter(x => x).map(x => x.split(",").map(is)) }

function settings(str=help, reg=/-\w+\s+(\w+)[^\n]*=\s*(\S+)/g) {
  let it = {},m 
  while (m = reg.exec(str)) it[m[1]] = is(m[2]); 
  return it }

const the = settings(help)

// Park-Miller random number generator
let SEED = the.rseed
function rand(n=1) {
  SEED = (16807 * SEED) % 2147483647
  return n * (SEED - 1) / 2147483646 }
  
//---------------------------------------------------------------------------------------
let eg={}

eg.eg = arg => console.log("--eg called with", arg)

eg.seed = function(_) { // does SEED reset mean same numbers, same order?
  let a=[], m=0
  SEED = the.rseed; for (let i = 0; i < 30; i++)      a[i] = round(rand(100));
  SEED = the.rseed; for (let i = 0; i < 30; i++) m += a[i] - round(rand(100));
  assert(m===0,"bad seed reset") }

eg.csv = (f) => csv(f || the.file).forEach( a => out(a.join(", "))) 

eg.cols = (_) => 
  Cols("name age Mpg+ Acc+ Lbs-".split(" ")).all.forEach(col => out(col))

eg.misc = function(_) {
	out(the)
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
	out(c.mid())
	out(c.var()) }

//---------------------------------------------------------------------------	-----------

if (require.main === module) cli(the, eg)
