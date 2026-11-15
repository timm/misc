#!/usr/bin/env python3
"""
sa: simulated annealing over labelled-as-you-go tabular data.
(c) 2026, Tim Menzies <timm@ieee.org>, MIT license
Kernel (csv, Tab, Num/Sym, dists) trimmed from ezr-py/xai.py.

USAGE: python3 sa.py [--key=val ...]

OPTIONS:
  --file   data file       = $MOOT/optimize/misc/auto93.csv
  --seed   random seed     = 1
  --val    holdout labels  = 50
  --pool   unlabelled cap  = 1000
  --budget new labels      = 30
  --temp   start temp      = 0.2
  --cool   cooling rate    = 0.9
  --how    sa | near | rand = sa

Design: 50 labelled holdout rows seed a best/rest cache (best =
top sqrt(n) by disty). SA mutates per x-column along the line
from rest's mid to best's mid, mass near best; realizes the
synthetic point as its nearest unlabelled pool row; scores it cheaply by the disty of its
nearest labelled neighbour (distx); on SA-accept, spends one true
label and feeds the cache. Returns the best row found.
"""
import os, re, sys, random
from math import exp, sqrt
from types import SimpleNamespace as o
from collections.abc import Iterator
TINY = 1e-32
rand = random.random
any  = random.choice   # shadows builtin; csv() avoids it
MOOT = os.environ.get("MOOT") or os.path.expanduser("~/gits/moot")

#-- kernel (from xai.py) ----------------------------------------
def thing(s) -> int|float|bool|str:
  if (s[1:] if s[:1]=="-" else s).isdigit(): return int(s)
  try: return float(s)
  except ValueError: return s=="True" or (s!="False" and s)

def settings(doc) -> o:
  pat = r"--(\w+)\s+[^=\n]*=\s*(\S+)"
  return o(**{k: thing(v) for k,v in re.findall(pat, doc)})

def csv(file) -> Iterator[list]:
  file = os.path.expanduser(file.replace("$MOOT", MOOT, 1))
  with open(file, encoding="utf-8") as f:
    for line in f:
      row = [x.strip() for x in line.split(",")]
      if "".join(row): yield [thing(x) for x in row]

Sym = dict
def Num(n=0, mu=0, m2=0) -> tuple: return (n, mu, m2)
def is_sym(i) -> bool: return isinstance(i, dict)
def sd(num: tuple) -> float: n,_,m2=num; return 0 if n<2 else (max(0,m2)/(n-1))**.5

def count(sym: dict, v, inc=1) -> dict:
  if (c := sym.get(v,0) + inc) > 0: sym[v] = c
  else: sym.pop(v, None)
  return sym

def welford(num: tuple, v, inc=1) -> tuple:
  n, mu, m2 = num
  if (n := n + inc) <= 0: return Num()
  d = v - mu; mu += inc * d / n
  return (n, mu, m2 + inc * d * (v - mu))

def add(col: tuple|dict, v, inc=1) -> tuple|dict:
  if v == "?": return col
  return (count if is_sym(col) else welford)(col, v, inc=inc)

def Tab(src) -> o:
  def init(names):
    cols,x,y,goal={},[],[],{}
    for at, s in enumerate(names):
      cols[at] = Num() if s[0].isupper() else Sym()
      if s[-1] == "X": continue
      if s[-1] in "+-": y += [at]; goal[at] = s[-1]=="+"
      else:             x += [at]
    return o(names=names,cols=cols,x=x,y=y,goal=goal,rows=[])

  src = iter(src)
  tbl = init(next(src))
  for row in src: addRow(tbl, row)
  return tbl

def addRow(tbl: o, row: list, inc=1) -> None:
  (tbl.rows.append if inc > 0 else tbl.rows.remove)(row)
  for at in tbl.cols: tbl.cols[at] = add(tbl.cols[at], row[at], inc)

def norm(num: tuple, v) -> float|str:
  if v == "?": return v
  z = (v - num[1]) / (sd(num) + TINY)
  return 1 / (1 + exp(-1.7 * max(-3, min(3, z))))

def minkowski(vals, p=2) -> float:
  tot = nn = 0
  for v in vals: tot += v**p; nn += 1
  return (tot / (nn or 1)) ** (1/p)

def gap(col: tuple|dict, u, v) -> float|bool:
  if u == v == "?": return 1
  if is_sym(col): return u != v
  u, v = norm(col, u), norm(col, v)
  if u == "?": u = 1 if v < .5 else 0
  if v == "?": v = 1 if u < .5 else 0
  return abs(u - v)

def disty(tbl: o, row: list) -> float:
  return minkowski(abs(norm(tbl.cols[a], row[a]) - tbl.goal[a])
                   for a in tbl.y if row[a] != "?")

def distx(tbl: o, r1: list, r2: list) -> float:
  return minkowski(gap(tbl.cols[a], r1[a], r2[a]) for a in tbl.x)

#-- best/rest cache ---------------------------------------------
# Labelled rows sorted by y; best = ~top sqrt(n). Newcomers join
# best; when best overflows, its worst is demoted to rest
# (welford/count with inc=-1). Demotion is one-way: see push.

def Cache(tbl: o) -> o:
  return o(tbl=tbl, all=[], best=Tab([tbl.names]), rest=Tab([tbl.names]))

def push(cache: o, row: list) -> None:
  """Newcomers start in best; overflow demotes best's worst to rest.
  (Approximate: a demotion is one-way, so when sqrt(n) grows the
  boundary can lag one item. Good enough for a fuzzy sampler.)"""
  cache.all += [row]
  addRow(cache.best, row)
  while len(cache.best.rows) > (int(sqrt(len(cache.all))) or 1):
    doomed = max(cache.best.rows, key=lambda r: disty(cache.tbl, r))
    addRow(cache.best, doomed, -1); addRow(cache.rest, doomed)

def nearest_y(cache: o, row: list) -> float:
  near = min(cache.all, key=lambda r: distx(cache.tbl, r, row))
  return disty(cache.tbl, near)

#-- sampling ----------------------------------------------------
# Mutate along the rest->best centroid line, favoring best. Why
# so coarse: best holds k=sqrt(n) rows, so its centroid is known
# only to sd/k^.5 = sd/n^.25 (n=80 -> +-sd/3); structure finer
# than that (spreads, interval boundaries) is mostly noise at
# these budgets. Trust the direction mb-mr only when it exceeds
# that resolution; do not schedule steps finer than it.

def mutate(cache: o, row: list, p=1.0) -> list:
  """Walk step: copy of row where each x cell, with probability
  p, is replaced by a value extrapolated between two best rows
  (t in -.25..1.25); the cache hides in here and nowhere else."""
  b1, b2 = any(cache.best.rows), any(cache.best.rows)
  out = row[:]
  for a in cache.tbl.x:
    u, v = b1[a], b2[a]
    if rand() > p or u == v == "?": continue
    if "?" in (u, v): out[a] = v if u == "?" else u
    elif is_sym(cache.tbl.cols[a]): out[a] = u if rand() < .5 else v
    else: out[a] = v + (rand()*1.5 - .25) * (u - v)
  return out

#-- baselines ---------------------------------------------------
def rando(tbl: o) -> o:
  "Baseline: spend the whole budget on random rows"
  c = Cache(tbl)
  for row in random.sample(tbl.rows, the.val + the.budget):
    push(c, row)
  return c

def near(tbl: o) -> o:
  """Baseline: classic acquire. Scan the pool; label the first
  item sitting nearer its best rows than its rest rows."""
  rows = random.sample(tbl.rows, len(tbl.rows))
  val, pool = rows[:the.val], rows[the.val:the.val+the.pool]
  c = Cache(tbl)
  for row in val: push(c, row)
  d = lambda rs, r: min(distx(tbl, r1, r) for r1 in rs)
  while len(c.all) - the.val < the.budget and pool:
    row = next((r for r in pool
                if d(c.best.rows, r) < d(c.rest.rows, r)), pool[0])
    pool.remove(row); push(c, row)
  return c

#-- sa ----------------------------------------------------------
def accept(y: float, ycur: float, t: float) -> bool:
  "The whole SA kernel: better, or lucky at this temperature"
  return y < ycur or rand() < exp(-(y - ycur)/(t + TINY))

def sa(tbl: o) -> o:
  rows = random.sample(tbl.rows, len(tbl.rows))
  val, pool = rows[:the.val], rows[the.val:the.val+the.pool]
  c = Cache(tbl)
  for row in val: push(c, row)                   # seed: holdout
  cur   = min(c.all, key=lambda r: disty(tbl, r))
  ycur, t = disty(tbl, cur), the.temp
  for _ in range(32 * the.budget):               # hard iteration cap
    if len(c.all) - the.val >= the.budget or not pool: break
    synth = mutate(c, cur)
    cand  = min(pool, key=lambda r: distx(tbl, synth, r))
    if accept(nearest_y(c, cand), ycur, t):      # cheap estimate..
      pool.remove(cand)                          # ..then pay a label
      push(c, cand)
      cur, ycur = cand, disty(tbl, cand)
    t *= the.cool
  return c

def main() -> None:
  for arg in sys.argv[1:]:
    if arg[:2]=="--" and "=" in arg:
      k,v = arg[2:].split("=",1)
      if k in vars(the): setattr(the, k, thing(v))
  random.seed(the.seed)
  tbl = Tab(csv(the.file))
  c = dict(sa=sa, near=near, rand=rando)[the.how](tbl)
  base = sorted(disty(tbl,r) for r in tbl.rows)
  best = min(c.best.rows, key=lambda r: disty(tbl,r))
  y0   = disty(tbl, best)
  print("rows %s  labels %s+%s  best %.3f  (corpus best %.3f, median %.3f)"
        % (len(tbl.rows), the.val, len(c.all) - the.val, y0, base[0], base[len(base)//2]))
  print("  " + ", ".join("%s=%s" % (n,v)
        for n,v in zip(tbl.names, best) if n[-1] != "X"))

the = settings(__doc__)
if __name__ == "__main__": main()
