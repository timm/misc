#!/usr/bin/env python3 -B
"""
core.py: shared structures, distance, stats, search core.
(c) 2026 Tim Menzies, timm@ieee.org, MIT license

Classes Num, Sym, Cols, Data carry methods. Type-dispatch lives in
the class hierarchy, not in module functions. Module-level shims
(add, mid, spread, etc.) delegate to methods for backward compat.

Shared options (used in >1 app):
    --seed=1             random number seed
    --p=2                Minkowski exponent
    --few=128            sample cap for cluster/acquire
    --show.show=30       tree display width
    --show.decimals=2    decimal places
    --stats.cliffs=0.195 Cliffs Delta threshold
    --stats.conf=1.36    KS test confidence
    --stats.eps=0.35     margin of error multiplier
    --bayes.m=2          m-estimate for Naive Bayes
    --bayes.k=1          k-estimate (Laplace) for NB
"""
from __future__ import annotations
from time import perf_counter_ns as now
import os, re, random, sys, bisect, math
from random import random as rand
from random import choices, choice, sample, shuffle
from math import log, log2, exp, sqrt, pi
from types import SimpleNamespace as S

isa = isinstance

# ---- Cols ----------------------------------------------------------
def Col(txt="", a=0):
  return (Num if txt[0].isupper() else Sym)(txt, a)

class Num:
  def __init__(i, txt="", a=0):
    i.txt, i.at, i.n = txt, a, 0
    i.mu = i.m2 = i.sd = 0
    i.heaven = txt[-1:] != "-"

  def add(i, v, w=1):
    if v == "?": return v
    if w < 0 and i.n <= 2:
      i.n = i.mu = i.m2 = i.sd = 0
    else:
      i.n  += w
      delta = v - i.mu
      i.mu += w * delta / i.n
      i.m2 += w * delta * (v - i.mu)
      i.sd  = sqrt(max(0, i.m2) / (i.n - 1)) if i.n > 1 else 0
    return v

  def sub(i, v): return i.add(v, w=-1)

  def mid(i):    return i.mu
  def spread(i): return i.sd

  def norm(i, v):
    if v == "?": return v
    z = max(-3, min(3, (v - i.mu) / (i.sd + 1e-32)))
    return 1 / (1 + exp(-1.7 * z))

  def aha(i, u, v):
    if u == v == "?": return 1
    u, v = i.norm(u), i.norm(v)
    u = u if u != "?" else (0 if v > 0.5 else 1)
    v = v if v != "?" else (0 if u > 0.5 else 1)
    return abs(u - v)

  def like(i, v, prior):
    sd = i.sd + 1e-32; z = 2*sd*sd
    return exp(-(v - i.mu)**2 / z) / sqrt(pi * z)

  def pick(i, v=None):
    tmp = v if v is not None and v != "?" else i.mu
    lo, hi = i.mu - 3*i.sd, i.mu + 3*i.sd
    new = tmp + i.sd * 2 * (rand() + rand() + rand() - 1.5)
    return lo + (new - lo) % (hi - lo + 1e-32)

class Sym:
  def __init__(i, txt="", a=0):
    i.txt, i.at, i.n, i.has = txt, a, 0, {}

  def add(i, v, w=1):
    if v == "?": return v
    i.has[v] = w + i.has.get(v, 0)
    return v

  def sub(i, v): return i.add(v, w=-1)

  def mid(i):    return mode(i.has)
  def spread(i): return entropy(i.has)

  def aha(i, u, v):
    if u == v == "?": return 1
    return u != v

  def like(i, v, prior):
    return ((i.has.get(v, 0) + the.bayes.k * prior)
            / (i.n + the.bayes.k))

  def pick(i, v=None): return pick(i.has)

def mode(d):    return max(d, key=d.get)
def entropy(d): n = sum(d.values()); return -sum(v/n*log2(v/n) for v in d.values())

class Cols:
  def __init__(i, names):
    i.names = names
    i.klass, i.xs, i.ys, i.all = None, [], [], []
    for j, txt in enumerate(names):
      i.all.append(col := Col(txt, j))
      if txt[-1] != "X":
        if txt[-1] == "!": i.klass = col
        (i.ys if txt[-1] in "+-!" else i.xs).append(col)

  def add(i, v, w=1):
    for col in i.all: col.add(v[col.at], w)

  def sub(i, v): i.add(v, w=-1)

# ---- Data ----------------------------------------------------------
class Data:
  def __init__(i, src=None):
    src = iter(src or [])
    i.rows, i._centroid = [], None
    i.cols = Cols(next(src))
    for row in src: i.add(row)

  def add(i, v, w=1):
    i._centroid = None
    i.cols.add(v, w)
    if w > 0: i.rows.append(v)
    else    : i.rows.remove(v)
    return v

  def sub(i, v): return i.add(v, w=-1)

  def clone(i, rows=None):
    d = Data([i.cols.names])
    for r in (rows or []): d.add(r)
    return d

  def mids(i):
    i._centroid = i._centroid or [c.mid() for c in i.cols.all]
    return i._centroid

  def disty(i, row):
    return minkowski((abs(y.norm(row[y.at]) - y.heaven)
                      for y in i.cols.ys), the.p)

  def distx(i, r1, r2):
    return minkowski((x.aha(r1[x.at], r2[x.at])
                      for x in i.cols.xs), the.p)

  def nearest(i, row, rows=None):
    return min(rows or i.rows, key=lambda r2: i.distx(row, r2))

  def picks(i, row, n=1):
    s = row[:]
    for col in sample(i.cols.xs, min(n, len(i.cols.xs))):
      s[col.at] = col.pick(s[col.at])
    return s

  def likes(i, row, n_rows, n_klasses):
    prior = ((len(i.rows) + the.bayes.m)
             / (n_rows + the.bayes.m * n_klasses))
    ls = [c.like(v, prior) for c in i.cols.xs
          if (v := row[c.at]) != "?"]
    return log(prior) + sum(log(v) for v in ls if v > 0)

  def wins(i):
    ys = sorted(i.disty(r) for r in i.rows)
    ten = len(ys) // 10
    lo, med, sd = ys[0], ys[5*ten], (ys[9*ten] - ys[ten]) / 2.56
    def f(row):
      x = i.disty(row)
      if x < lo + 0.35*sd: x = lo
      return max(-100, int(100 * (1 - (x - lo) / (med - lo + 1e-32))))
    return f

# ---- Distance helper ----------------------------------------------
def minkowski(items, p=2):
  tot, n = 0, 1e-32
  for v in items: tot, n = tot + v**p, n + 1
  return (tot/n) ** (1/p)

# ---- adds (fold values into accumulator; works on any class) -----
def adds(src, it=None):
  it = it if it is not None else Num()
  for v in (src or []): it.add(v)
  return it

# ---- Module shims (delegate to methods; keep old module API) -----
def add(it, v, w=1):           return it.add(v, w)
def sub(it, v):                return it.sub(v)
def clone(d, rows=None):       return d.clone(rows)
def mids(d):                   return d.mids()
def mid(c):                    return c.mid()
def spread(c):                 return c.spread()
def norm(c, v):                return c.norm(v)
def aha(c, u, v):              return c.aha(u, v)
def like(c, v, prior):         return c.like(v, prior)
def disty(d, row):             return d.disty(row)
def distx(d, r1, r2):          return d.distx(r1, r2)
def nearest(d, row, rows=None): return d.nearest(row, rows)
def wins(d):                   return d.wins()
def likes(d, row, n, k):       return d.likes(row, n, k)
def picks(d, row, n=1):        return d.picks(row, n)

def pick(it, v=None):
  """Polymorphic: dict (weighted), Num, Sym, or call obj.pick()."""
  if isinstance(it, (Num, Sym)): return it.pick(v)
  if isinstance(it, dict):
    n = sum(it.values()) * rand()
    for k, v in it.items():
      if (n := n - v) <= 0: break
    return k

# ---- (1+1) search core ---------------------------------------------
def oneplus1(data, mutate, accept, oracle, budget=1000, restart=0):
  h, best, best_e = 0, None, 1e32
  s, e, imp = choice(data.rows)[:], 1e32, 0
  while h < budget:
    for sn in mutate(s):
      h += 1
      en = oracle(sn)
      if accept(e, en, h, budget): s, e = sn, en
      if en < best_e:
        best, best_e, imp = sn[:], en, h
        yield h, best_e, best
      if restart and h - imp > restart:
        s = choice(data.rows)[:]
        e, imp = 1e32, h
        break

def oracleNearest(data, row):
  near = data.nearest(row)
  for c in data.cols.ys: row[c.at] = near[c.at]
  return data.disty(row)

def last(gen):
  v = None
  for v in gen: pass
  return v

# ---- Stats --------------------------------------------------------
def same(xs, ys, eps):
  xs, ys = sorted(xs), sorted(ys)
  n, m = len(xs), len(ys)
  if abs(xs[n//2] - ys[m//2]) <= eps: return True
  gt = sum(bisect.bisect_left(ys, a)      for a in xs)
  lt = sum(m - bisect.bisect_right(ys, a) for a in xs)
  if abs(gt - lt) / (n*m) > the.stats.cliffs: return False
  ks = lambda v: abs(bisect.bisect_right(xs, v)/n
                   - bisect.bisect_right(ys, v)/m)
  return max(max(map(ks, xs)), max(map(ks, ys))) \
         <= the.stats.conf * ((n+m)/(n*m))**.5

def bestRanks(d):
  items = sorted(d.items(),
                 key=lambda kv: sorted(kv[1])[len(kv[1])//2])
  k0, lst0 = items[0]
  best = {k0: adds(lst0, Num(k0))}
  for k, lst in items[1:]:
    if same(lst0, lst, best[k0].spread() * the.stats.eps):
      best[k] = adds(lst, Num(k))
    else: break
  return best

# ---- Utilities ----------------------------------------------------
def dinc(k1, k2, b4=None):
  b4 = b4 or {}; b4[k1] = b4.get(k1) or {}
  b4[k1][k2] = b4[k1].get(k2, 0) + 1
  return b4

def o(x):
  if isa(x, float): return f"{x:.{the.show.decimals}f}"
  if isa(x, dict):
    return "{" + ", ".join(f"{k}={o(v)}" for k, v in sorted(x.items())) + "}"
  if isa(x, list):
    return "{" + ", ".join(map(o, x)) + "}"
  if isa(x, S): return "S" + o(x.__dict__)
  if hasattr(x, "__dict__"):
    return x.__class__.__name__ + o(x.__dict__)
  return str(x)

def table(lst, w=10):
  if not lst: return
  ds = [x if type(x) is dict else x.__dict__ for x in lst]
  ks = list(ds[0].keys())
  print("".join(f"{str(k):>{w}}" for k in ks))
  print("-" * (len(ks) * w))
  for d in ds:
    print("".join(f"{str(d.get(k, '')):>{w}}" for k in ks))

def thing(txt):
  def _bool(s): return {"true":1, "false":0}.get(s.lower(), s)
  txt = txt.strip()
  for f in (int, float, _bool):
    try: return f(txt)
    except ValueError: pass

def nest(t, k, v):
  for x in (ks := k.split("."))[:-1]:
    t = t.__dict__.setdefault(x, S())
  setattr(t, ks[-1], v)

def csv(f, clean=lambda txt: txt.partition("#")[0].split(",")):
  with open(f, encoding="utf-8") as file:
    for txt in file:
      row = clean(txt)
      if any(x.strip() for x in row):
        yield [thing(x) for x in row]

def ready(file):
  d = file if isinstance(file, Data) else Data(csv(file))
  shuffle(d.rows)
  half = len(d.rows) // 2
  return d, d.clone(d.rows[:half][:the.few]), d.rows[half:]

# ---- Config + CLI -------------------------------------------------
the = S()
def loadDoc(doc):
  for k, v in re.findall(r"([\w.]+)=(\S+)", doc or ""):
    nest(the, k, thing(v))

loadDoc(__doc__)

def cli():
  main = sys.modules["__main__"]
  args = sys.argv[1:]
  while args:
    random.seed(the.seed)
    k = re.sub(r"^-+", "", args.pop(0))
    fn = getattr(main, f"test_{k}", None)
    if fn:
      fn(*[thing(args.pop(0)) for _ in fn.__annotations__ if args])
    else:
      nest(the, k, thing(args.pop(0)))

__all__ = [
  "S", "isa", "now", "rand", "choices", "choice", "sample", "shuffle",
  "log", "log2", "exp", "sqrt", "pi",
  "the", "loadDoc", "cli",
  "Col", "Num", "Sym", "Cols", "Data",
  "add", "sub", "adds", "clone", "mid", "mids", "mode", "spread",
  "entropy", "norm",
  "minkowski", "disty", "distx", "aha", "nearest", "wins",
  "like", "likes",
  "pick", "picks",
  "oneplus1", "oracleNearest", "last",
  "same", "bestRanks",
  "dinc", "o", "table", "thing", "nest", "csv", "ready",
]
