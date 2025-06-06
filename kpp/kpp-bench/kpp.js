#!/usr/bin/env node
fs = require("fs")
seedrandom = require("seedrandom")
Math.random = seedrandom(1234567891)

the = { p: 2, Few: 32, Stop: 4, file: "big.csv" }

let rows = [],
    lo = {},
    hi = {},
    xcols = {},
    names = []

atom  = x => isNaN(x = +x) ? (x == "true" ? true : x == "false" ? false : x) : x
split = s => s.split(",").map(atom)
sum   = t => t.reduce((a, b) => a + b, 0)
any   = t => t[Math.floor(Math.random() * t.length)]
norm  = (c, x) => (x - lo[c]) / ((hi[c] - lo[c]) + 1e-32)

xdist = (r1, r2) => {
  let d = 0, n = 0
  for (let c in xcols) {
    let a = norm(c, r1[c]),
        b = norm(c, r2[c])
    d += Math.abs(a - b) ** the.p
    n++ }
  return (d / n) ** (1 / the.p) }

update = (j, x) => {
  if (xcols[j]) {
    lo[j] = lo[j] === undefined ? x : Math.min(lo[j], x)
    hi[j] = hi[j] === undefined ? x : Math.max(hi[j], x) }
  return x }

readHeader = hdr =>
  hdr.map((name, j) => {
    if (!/[+-]$/.test(name)) xcols[j] = true
    return name })

readCSV = file => {
  lines = fs.readFileSync(file, "utf-8").split(/\r?\n/).filter(Boolean)
  names = readHeader(lines[0].split(","))
  rows = lines.slice(1).map(line => split(line).map(update)) }

kpp = (k = the.Stop, r = rows) => {
  let c = [any(r)],
      s = r.slice(0, the.Few)
  for (let i = 1; i < k; i++) {
    let d = s.map(x => Math.min(...c.map(y => xdist(x, y) ** 2)))
    let r = Math.random() * sum(d)
    for (let j = 0; j < d.length; j++) {
      if ((r -= d[j]) <= 0) {
        c.push(s.splice(j, 1)[0])
        break } } }
  return c }

readCSV(the.file)
kpp().forEach(r => {}) // output suppressed for timing

