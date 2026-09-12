#!/usr/bin/env python3 -B
"""
tar4.py: Num, Sym, Tbl and create-from-disk routines
(c) 2026 Tim Menzies <timm@ieee.org> MIT license
(extracted from ezr.py)

Options:

      -P=2             minkowski coefficient
      -Bins=7          bins per numeric column
      -Best=2          best = ydist bin below this
      -Top=20          bins/rules to report
      -Rules=1000      random rules tried per round
      -Lives=3         grow: rounds without improvement
      -Few=128         max train rows
      -Start=4         acquire: initial random labels
      -Stop=50         acquire: total labelling budget
      -Check=5         holdout: top picks to label
      -Repeats=20      holdout: number of train/test splits
      -Seed=1234567891 random number seed
"""

# pylint: disable=bad-indentation,invalid-name
# pylint: disable=missing-function-docstring
# pylint: disable=multiple-statements,multiple-imports
# pylint: disable=unnecessary-lambda-assignment
# pylint: disable=dangerous-default-value
# pylint: disable=unidiomatic-typecheck

import os, random, re, sys, time
from math import exp, log2, sqrt

#-- misc --------------------------------------------------
def say(x, p=2):
  if type(x) is float: x= f"{x:.{p}f}".rstrip("0").rstrip(".")
  elif type(x) in (dict, o): x= "{" + ", ".join(
    f"{k}: {say(x[k], p)}" for k in x if str(k)[0] != "_") + "}"
  elif type(x) in (list,tuple):
    x = "[" + ", ".join(say(v, p) for v in x) + "]"
  return str(x)

class o(dict):
  __repr__ = say
  __getattr__,__setattr__ = dict.__getitem__,dict.__setitem__

#-- disk --------------------------------------------------
def atom(s,bools={'True': True, 'False': False}):
  try: return int(s)
  except ValueError:
    try: return float(s)
    except ValueError:
      s = s.strip()
      return bools[s] if s in bools else s

def csv(file):
  file = file.replace("$MOOT", os.environ.get("MOOT")
                      or os.path.expanduser("~/gits/moot"), 1)
  with open(file, encoding="utf-8") as f:
    return [tuple(atom(x) for x in line.split(","))
            for line in f if line.strip()]

#-- settings ----------------------------------------------
the = o(_defaults=o())
for k, v in re.findall(r"(\w+)=(\S+)", __doc__ or ""):
  the[k] = the._defaults[k] = atom(v)

#-- structs -----------------------------------------------
Num = lambda: (0, 0, 0) # n, mu, m2: all Welford keeps
Sym = dict

type Atom = str | bool | int | float
type Col  = tuple[int, float, float] | dict # Num | Sym
type Row  = tuple[Atom, ...]
type Rows = list[Row]
type Tbl  = o # rows:Rows, cols:{at:Col}, x:[at],
              # y:{at:bool}, names:Row, klass:at|None

def sd(col): return 0 if col[0] < 2 else sqrt(col[2]/(col[0]-1))

def add(col, v, inc=1): # new Num, or updated Sym; inc=-1 undoes
  if v == "?": return col
  if type(col) is Sym: col[v] = col.get(v, 0) + inc; return col
  n, mu, m2 = col
  n += inc
  d = v - mu
  mu += inc * d / max(1, n)
  return (n, mu, max(0, m2 + inc * d * (v - mu)))

def adds(lst, it=None): # accumulate a list into it
  if it is None: it = Num()   # NB: "it or Num()" would
  for y in lst: it = add(it, y)  # clobber an empty Sym()
  return it

def size(col):
  return sum(col.values()) if type(col) is Sym else col[0]

def div(col): # Num: sd. Sym: entropy
  if type(col) is not Sym: return sd(col)
  n = sum(col.values())
  return -sum(v/n * log2(v/n) for v in col.values() if v>0)

def mid(col):
  return max(col, key=col.get) if type(col) is Sym else col[1]

def addRow(tbl, row=None, inc=1): # inc=-1 pops the last row
  tbl._mids = None
  if inc > 0: tbl.rows.append(row)
  else: row = tbl.rows.pop()
  for at in tbl.cols:
    tbl.cols[at] = add(tbl.cols[at], row[at], inc)
  return row

def Tbl(src):
  tbl = o(rows=[], cols={}, x=[], y={}, names=src[0],
          klass=None, _mids=None)
  for at, s in enumerate(tbl.names):
    if not s.endswith("X"):
      tbl.cols[at] = Num() if s[0].isupper() else Sym()
      if   s[-1] == "!":  tbl.klass = at
      elif s[-1] in "+-": tbl.y[at] = s[-1] == "+"
      else: tbl.x.append(at)
  for row in src[1:]: addRow(tbl, row)
  return tbl

def clone(tbl, rows=[]): return Tbl([tbl.names] + rows)

#-- distance ----------------------------------------------
def norm(col, v):
  z = max(-3, min(3, (v - col[1]) / (1e-32 + sd(col))))
  return 1 / (1 + exp(-1.7 * z))

def ydist(tbl, row):
  return (sum(abs(norm(tbl.cols[at], row[at]) - w) ** the.P
             for at, w in tbl.y.items()) / len(tbl.y))**(1/the.P)

def mids(tbl): # x centroid; cached until rows change
  tbl._mids = tbl._mids or {at:mid(tbl.cols[at]) for at in tbl.x}
  return tbl._mids

def _dist(col, a, b):
  if a == "?" or b == "?": return 1
  return (a != b if type(col) is Sym
          else abs(norm(col, a) - norm(col, b)))

def xdist(tbl, row, m):
  return (sum(_dist(tbl.cols[at], row[at], m[at]) ** the.P
              for at in tbl.x) / len(tbl.x)) ** (1 / the.P)

#-- acquire -----------------------------------------------
def centroid(tbl, best, rest): # near best, far from rest
  return lambda z: (xdist(tbl, z, mids(rest))
                  - xdist(tbl, z, mids(best)))

def label(tbl, best, rest, row): # keep best pool near sqrt
  addRow(best, row)
  best.rows.sort(key=lambda r: ydist(tbl, r))
  b, r = len(best.rows), len(rest.rows)
  if b > sqrt(1 + b + r): addRow(rest, addRow(best, inc=-1))

def acquire(tbl, cap=None, score=centroid): # pop the top scorer
  best, rest = clone(tbl), clone(tbl)
  todo = random.sample(tbl.rows, len(tbl.rows))[:the.Few]
  for _ in range(the.Start): label(tbl, best, rest, todo.pop())
  cap = cap or the.Stop
  while todo and len(best.rows) + len(rest.rows) < cap:
    todo.sort(key=score(tbl, best, rest))
    label(tbl, best, rest, todo.pop())
  return best.rows + rest.rows

#-- bins --------------------------------------------------
def bin(col, v): # Sym: itself. Num: bucket of norm(), 0..Bins-1
  if v != "?":
    return v if type(col) is Sym else int(norm(col, v) * the.Bins)

def xbins(tbl, row): # {x: bin}; "?" left out
  return {x: b for x in tbl.x
            if (b := bin(tbl.cols[x], row[x])) is not None}

def bins(tbl, rows=None): # row bitmasks: ms[(x,bin)], best, all, n
  rows = rows or tbl.rows
  ys   = adds(ydist(tbl, r) for r in rows)
  bm   = o(ms={}, best=0, all=(1 << len(rows)) - 1, n={True:0, False:0})
  for i, row in enumerate(rows):
    best = bin(ys, ydist(tbl, row)) < the.Best
    bm.n[best] += 1
    if best: bm.best |= 1 << i
    for x, b in xbins(tbl, row).items():
      bm.ms[(x, b)] = bm.ms.get((x, b), 0) | (1 << i)
  return bm

def score(bm, m): # b^2/(b+r); b,r = ratio of best,rest rows in mask m
  b = (m &  bm.best).bit_count() / max(1, bm.n[True])
  r = (m & ~bm.best & bm.all).bit_count() / max(1, bm.n[False])
  return b*b / (b + r + 1e-32)

def scores(bm, top=None): # top single ranges
  return sorted(((score(bm, m), x, b) for (x, b), m in bm.ms.items()),
                reverse=True)[:top or the.Top]


#-- rules -------------------------------------------------
# rule = {x: {bin,...}}: AND across x, OR within x
def matches(rule, bx): # AND over x, OR within x; missing x fails
  return all(bx.get(x) in bs for x, bs in rule.items())

def ruleMask(bm, rule): # rows matching rule: AND over x of OR over bins
  m = bm.all
  for x, bs in rule.items():
    ors = 0
    for b in bs: ors |= bm.ms.get((x, b), 0)
    m &= ors
  return m

def ruleScore(bm, rule): return score(bm, ruleMask(bm, rule))

def rule(ranges): # [(x,b),...] -> {x: {b,...}}
  out = {}
  for x, b in ranges: out.setdefault(x, set()).add(b)
  return out

def rules(bm, pool, mx): # best of Rules random subsets, size <= mx
  seen = {}
  for _ in range(the.Rules):
    rs = frozenset(random.sample(pool, random.randint(1, min(mx, len(pool)))))
    if rs not in seen: seen[rs] = ruleScore(bm, rule(rs))
  return sorted(seen.items(), key=lambda z: -z[1])[:the.Top]

def grow(tbl, bm): # widen rules; lose a life per non-improvement
  pool, best, lives = [(x, b) for _, x, b in scores(bm)], None, the.Lives
  for mx in range(1, len(tbl.x) + 1):
    top = rules(bm, pool, mx)
    if best and top[0][1] <= best[0][1]:
      lives -= 1
      if lives <= 0: break
    else: best = top
    pool = list({r for rs, _ in top for r in rs})
  return best

def showRule(tbl, rs):
  return " & ".join(f"{tbl.names[x]}={sorted(bs)}"
                    for x, bs in sorted(rule(rs).items()))

#-- holdout -----------------------------------------------
mean   = lambda ys: sum(ys) / len(ys)

def wins(tbl, b4=mean): # b4 anchors win=0; best row anchors 100
  ys = sorted(ydist(tbl, r) for r in tbl.rows)
  lo, b4 = ys[0], b4(ys)
  return lambda r: max(-100, min(100,
    100 * (1 - (ydist(tbl, r) - lo) / (b4 - lo + 1e-32))))

def guess(top, bx): # bigger = better; 0 = matched no rule
  return sum(s for rs, s in top if matches(rule(rs), bx))

def holdout(tbl): # rules from acquired rows; label Check best test rows
  rows = random.sample(tbl.rows, len(tbl.rows))
  n = len(rows) // 2
  train, test = rows[:n][:the.Few], rows[n:]
  tr  = clone(tbl, train)
  lab = acquire(tr, the.Stop - the.Check)
  top = grow(tr, bins(tr, lab))
  pick = sorted(test, key=lambda r: -guess(top, xbins(tr, r)))[:the.Check]
  return min(pick, key=lambda r: ydist(tr, r))

#-- tests -------------------------------------------------
def test_num():
  "Welford add matches textbook mean and sd"
  c = adds([2, 4, 4, 4, 5, 5, 7, 9])
  assert c[0] == 8 and c[1] == 5 and abs(sd(c)-2.138) < .01
  print(f"mu {say(c[1])} sd {say(sd(c))}")

def test_sym():
  "Syms count; mid is mode; div is entropy"
  c = adds("aabbbc", Sym())
  assert c["b"]==3 and mid(c)=="b" and abs(div(c)-1.459)<.01
  print(f"mode {mid(c)} ent {say(div(c))}")

def test_tbl():
  "Headers route columns to x, y, klass, or nowhere"
  t = Tbl([("Age","job!","SkipX","Weight-"), (2,"a",3,80)])
  assert t.x == [0] and t.y == {3: False} and t.klass == 1
  assert 2 not in t.cols
  print(f"x {t.x} y {t.y} klass {t.klass}")

def _file(): return (sys.argv[1] if len(sys.argv) > 1
                     else "$MOOT/optimize/misc/auto93.csv")

def test_csv():
  "Load a csv from disk into a Tbl"
  t = Tbl(csv(_file()))
  print(f"n={len(t.rows)} x={t.x} y={t.y} klass={t.klass}")

def test_bins():
  "Top bins scored by b^2/(b+r)"
  t = Tbl(csv(_file()))
  bm = bins(t)
  print(f"best={bm.n[True]} rest={bm.n[False]}")
  for s, x, b in scores(bm):
    print(f"  {say(s):>5} {t.names[x]:<12} {b}")

def test_rules():
  "Grow rules from top bins; report best, with runtimes"
  t = Tbl(csv(_file()))
  t0 = time.perf_counter(); bm = bins(t)
  t1 = time.perf_counter(); top = grow(t, bm)
  t2 = time.perf_counter()
  for rs, s in top[:5]: print(f"  {say(s):>5} {showRule(t, rs)}")
  print(f"  secs: bins {t1-t0:.3f} grow {t2-t1:.3f}"
        f" rows {len(t.rows)} x {len(t.x)} rules {the.Rules}")

def test_holdout():
  "Mean win over Repeats train/test holdouts"
  t = Tbl(csv(_file()))
  win = wins(t)
  t0 = time.perf_counter()
  ws = [win(holdout(t)) for _ in range(the.Repeats)]
  print(f"win {round(mean(ws))} (min {round(min(ws))} max {round(max(ws))})"
        f" secs {time.perf_counter()-t0:.2f}")

if __name__ == "__main__":
  random.seed(the.Seed)
  for f in (test_num, test_sym, test_tbl, test_csv,
            test_bins, test_rules, test_holdout): f()
