#!/usr/bin/env python3 -B
"""fft1.py, fastmap bi-cluster with sd-plateau stop
(c) 2025, Tim Menzies <timm@ieee.org>, MIT license

Options:
 -s seed   seed=1234567891
 -t trees  trees=20
 -F frac   frac=0.1
 -f file   file=auto93.csv
"""
import random, math, sys, re
from types import SimpleNamespace as o

BIG = math.inf
the = o(seed=1234567891, trees=20, frac=0.1, stop=None, file="auto93.csv")

def coerce(s):
  try: return int(s)
  except:
    try: return float(s)
    except: return s.strip()

# ## memo --------------------------------------------------------
def memo(fn):
  d = {}
  def go(*args):
    k = tuple(sorted(id(a) for a in args))
    if k not in d: d[k] = fn(*args)
    return d[k]
  return d, go

# ## constructors -------------------------------------------------
def Num(at=0, txt=""):
  return o(it=Num, at=at, txt=txt, n=0, mu=0, m2=0, sd=0,
           lo=BIG, hi=-BIG, heaven=0 if txt.endswith("-") else 1)

def Sym(at=0, txt=""): return o(it=Sym, at=at, txt=txt, n=0, has={})

def Data():
  d = o(it=Data, cols=None, rows=[])
  d.xs, d.distx = memo(lambda r1, r2: _distx(d, r1, r2))
  d.ys, d.disty = memo(lambda r:      _disty(d, r))
  return d

# ## add (one polymorphic) ----------------------------------------
def add(it, v):
  if v == "?": return v
  if it.it is Sym:
    it.n += 1; it.has[v] = it.has.get(v, 0) + 1
  elif it.it is Num:
    it.n += 1; d = v - it.mu
    it.mu += d / it.n
    it.m2 += d * (v - it.mu)
    it.sd = (it.m2/(it.n-1))**0.5 if it.n > 1 else 0
    if v < it.lo: it.lo = v
    if v > it.hi: it.hi = v
  else:
    it.xs.clear(); it.ys.clear()
    if it.cols is None: it.cols = _header(v)
    else:
      it.rows.append(v)
      for c in it.cols.all: add(c, v[c.at])
  return v

def _header(names):
  cols = o(all=[], x=[], y=[], names=names)
  for at, s in enumerate(names):
    cols.all += [(Num if s[:1].isupper() else Sym)(at, s)]
    if   s[-1:] in "-+!": cols.y += [cols.all[-1]]
    elif s[-1:] != "X"  : cols.x += [cols.all[-1]]
  return cols

def read(file):
  for ln in open(file):
    ln = ln.strip()
    if ln and ln[0] != "#":
      yield [coerce(x.strip()) for x in ln.split(",")]

def adds(src, it=None):
  it = it or Data(); [add(it, x) for x in src]; return it

def clone(root, rows):
  return adds(rows, adds([root.cols.names]))

# ## metrics ------------------------------------------------------
def norm(c, v): 
  return v if v=="?" else (v - c.lo) / (c.hi - c.lo + 1E-32)

def _missNorm(c, v1, v2):
  v1,v2 = norm(c,v1), norm(c,v2)
  v1 = v1 if v1 != "?" else (0 if v2 > .5 else 1)
  v2 = v2 if v2 != "?" else (0 if v1 > .5 else 1)
  return v1, v2

def _distxCol(c, v1, v2):
  if v1 == "?" and v2 == "?": return 1
  if c.it is Sym: return 0 if v1 == v2 else 1
  v1, v2 = _missNorm(c, v1, v2)
  return (v1 - v2)**2

def _distx(d, r1, r2):
  s, n = 0, 0
  for c in d.cols.x:
    n += 1; s += _distxCol(c, r1[c.at], r2[c.at])
  return (s/n)**0.5

def _disty(d, r):
  s, n = 0, 0
  for c in d.cols.y:
    if c.it is Num and r[c.at] != "?":
      n += 1; s += (norm(c, r[c.at]) - c.heaven)**2
  return (s/n)**0.5 if n else 0

def ySd(d):
  ys = [c for c in d.cols.y if c.it is Num]
  return sum(c.sd for c in ys) / max(1, len(ys))

# ## fastmap ------------------------------------------------------
def far(d, rs, ref):
  rs.sort(key=lambda r: d.distx(ref, r))
  return rs[int(0.99 * len(rs))]

def fastmap(root, stop=None):
  stop = stop or the.stop or int(math.sqrt(len(root.rows)))
  leaves = []
  def go(sub, prev_sd):
    rs = list(sub.rows)
    cur_sd =  [c for c in sub.cols.y if c.it is Num]
    if len(rs) <= stop:
      leaves.append(sub); return
    a = random.choice(rs); b = far(sub, rs, a); a = far(sub, rs, b)
    rs.sort(key=lambda r: sub.distx(r, a)**2 - sub.distx(r, b)**2)
    m = len(rs) // 2
    go(clone(root, rs[:m]), cur_sd)
    go(clone(root, rs[m:]), cur_sd)
  go(root, BIG)
  return leaves

# ## jaccards -----------------------------------------------------
def _muDisty(root, leaf):
  return sum(root.disty(r) for r in leaf.rows) / max(1, len(leaf.rows))

def topLeaves(root, leaves, frac):
  arr = sorted(leaves, key=lambda L: _muDisty(root, L))
  k = max(1, int(len(arr) * frac))
  return [set(id(r) for r in L.rows) for L in arr[:k]]

def jac(a, b): return len(a & b) / len(a | b)

def jaccards(root, N=None, frac=None):
  N, frac = N or the.trees, frac or the.frac
  parts = [topLeaves(root, fastmap(root), frac) for _ in range(N)]
  out = []
  for i, p in enumerate(parts):
    for s in p:
      bests = [max(jac(s, t) for t in q)
               for j, q in enumerate(parts) if j != i]
      out.append(sum(bests) / len(bests))
  return sorted(out)

# ## main ---------------------------------------------------------
for n, v in enumerate(sys.argv):
  m = re.match(r"^-([stFfS])$", v)
  if m:
    setattr(the, {"s":"seed","t":"trees","F":"frac","f":"file",
                  "S":"stop"}[m.group(1)],
            coerce(sys.argv[n+1]))

if __name__ == "__main__":
  random.seed(the.seed)
  data = adds(read(the.file))
  if len(data.rows) > 2000:
    random.shuffle(data.rows); data.rows = data.rows[:2000]
  for j in jaccards(data): print(int(j*100))
