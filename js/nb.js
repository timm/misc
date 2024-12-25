const fs = require('fs');
const readline = require('readline');

say = console.log
min = Math.min; max = Math.max

class Sym {
  constructor(name="", pos=0) {
    this.n    = 0
    this.pos  = pos
    this.name = name
    this.most = 0
    this.mode = null
    this.all  = {}}

 add(x) {
   if (x === "?") return x
   this.n++
   let tmp = this.all[x] = (this.all[x] || 0) + 1
   if (tmp > this.most) {
     this.most=tmp; this.mode =x }
   return x }

  mid() { return this.mode }

  spread() {
    let n=0
    let a=self.all
    for (let k in self.all) { n -= a[k]/self.n * Math.log(a[k]/self.n,2) }
    return n }
}

class Num {
  constructor(name="", pos=0) {
    this.n    = 0
    this.pos  = pos
    this.name = name
    this.goal = name.at(-1) == "-" ? 0 : 1 
    this.mu   = this.m2 = this.sd = 0
    this.lo   = Infinity
    this.hi   = -Infinity }

  add(x) {
    if (x === "?") return x
    this.n++
    x = +x
    this.lo  = min(this.lo, x)
    this.hi  = max(this.hi, x)
    let d    = x - this.mu
    this.mu += d / this.n
    this.m2 += d * (x - this.mu) 
    this.sd  = this.n < 2 ? 0 : (this.m2/(this.n - 1))**.5
    return x }

  mid() { return this.mu }

  spread() { return this.sd }
}

class Cols {
  constructor(names) {
    this.klass = null
    this.names = names
    this.x     = []
    this.y     = []
    this.all   = []
    names.forEach((txt,pos) => {
      let col = push(this.all, (/^[A-Z]/.test(txt) ? Num : Sym)(txt,pos))
      if (txt.at(-1) != "X") {
        push(/[!+-]$/.test(txt) ? this.y : this.x, col)
        if (txt.at(-1) == "!") this.klass = col }})}}

// ----------------------------------------------------------------------------
class Data {
  constructor() {
    this.dep = {};
    this.indep = {};
    this.klass = {};
    this.rows = [];
  }

  addRow(row) {
    this.rows.push(row);
    this.updateNums(row);
  }

  updateNums(row) {
    row.forEach((val, col) => {
      if (this.indep[col] || this.dep[col]) {
        this.indep[col]?.add(val);
        this.dep[col]?.add(val);
      }
    });
  }

  addHeader(header) {
    header.forEach((val, col) => {
      if (val !== "?") {
        const isNum = !val.match(/\$/);
        const isDep = val.match(/\!/);
        const obj = new Num(val);

        if (isNum) {
          if (isDep) this.dep[col] = obj;
          else this.indep[col] = obj;
        } else {
          this.klass[col] = val;
        }
      }
    });
  }

  classify(row) {
    // Dummy classifier: Adjust this for real classification logic
    say(`Classifying row: ${row.join(",")}`);
    return true;  // Always returns true for now
  }
}

function readcsv(file) {
  const data = new Data();
  let row = 0;
  let buffer = [];

  const lines = readFile(file).split('\n');

  for (let str of lines) {
    str = clean(str);
    if (!str) continue;

    const a = str.split(",");

    if (row === 0) {
      data.addHeader(a);
    } else {
      buffer.push(a);

      if (row > 20) {
        const toClassify = buffer.shift();
        if (data.classify(toClassify)) {
          data.addRow(toClassify);
        }
      }
    }
    row++;
  }

  // Process remaining rows in the buffer
  buffer.forEach((line) => data.addRow(line));
  return data;
}

function readFile(file) {
  return `
    name,age!,salary$
    John,25,50000
    Jane,30,60000
    # comment
    Mike,22,?
    Sarah,29,70000
    Adam,24,?
    Chris,33,65000
    Emily,31,72000
    Eric,40,80000
    Anna,27,58000
    Liam,45,90000
    Eva,23,?
    Tom,38,73000
    Ruby,34,71000
    Mark,26,61000
    Nora,28,64000
    Leo,37,78000
    Zoe,32,77000
    Max,29,67000
    Mia,35,75000
    Jack,36,76000
    Amy,30,?
  `;
}

function isUpper(x) { return /[A-Z]/.test(x) }

function clean(str) {
  if (!str || str.match(/^[ \t]*$/) || str.startsWith("#")) return "";
  return str.replace(/[ \t\r\n]*$/g, "").replace(/#.*$/g, "");
}

function arrayIt(input) {
  yield return (typeof input === 'string') ?  csv(input) : items(item) }

function* items(arrayOfArrays) {
  for (const arr of arrayOfArrays) { yield arr }}

async function* csv(file) {
  const fileStream = fs.createReadStream(file);
  const rl = readline.createInterface({
    input: fileStream,
    crlfDelay: Infinity });
  for await (const line of rl) {
    const trimmed = line.trim();
    if (trimmed) {
      yield trimmed.split(','); }}}

// Start parsing
say(readcsv("nb.awk"))
