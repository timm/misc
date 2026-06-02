#!/usr/bin/env python3 -B
"""
tree.py, standalone experimental tree: node split tries ALL x-cols
(best global cut = CART-ish), not one random col. Self-contained
(own Num/Sym/Data/Cols + config) so we can iterate without fft1.
Next: feed it Fastmap dims instead of raw cols (dims:trees).

Histogram cut: binOf bins each col into a Num of yfun(row), sweep
with merge/unmerge for the min child-variance (L.m2+R.m2) cut.

Options:
 -s --seed  random seed       seed=1234567891
 -p --p     distance exponent p=2
 -R --Round repr decimals     Round=2
 -S --stop  leaf size         stop=None
 -d --Dims  projection dims   Dims=5
 -b --Bins  divisions per dim Bins=5
 -n --N     demo row sample   N=50
 -f --file  data file
            file=/Users/timm/gits/moot/optimize/misc/auto93.csv

eg: python3 tree.py -f FILE --tree
"""
import random, math, sys, re
from functools import reduce
BIG = math.inf

# ## lib --------------------------------------------------------
class o(dict):
  __getattr__ = dict.get; __setattr__ = dict.__setitem__
  def __repr__(i):
    return "{" + " ".join(":%s %s" % (k, shrink(i[k])) for k in i
                          if str(k)[0] != "_") + "}"

def shrink(v):                  # round floats for display (the.Round)
  if isinstance(v, float):
    return int(v) if v == int(v) else round(v, the.Round)
  return v

# ## constructors -----------------------------------------------
def Num(at=0, txt=""):
  return o(it=Num, at=at, txt=txt, n=0, mu=0, m2=0, sd=0,
           lo=BIG, hi=-BIG, goal=0 if txt.endswith("-") else 1)

def Sym(at=0, txt=""):
  return o(it=Sym, at=at, txt=txt, n=0, has={})

def Data(src=[]):
  return adds(src, o(it=Data, cols=None, rows=[]))

def Cols(names):
  cols, x, y, klass = [], [], [], None
  for at, s in enumerate(names):
    z = s[-1]
    cols += [col := (Num if s[0].isupper() else Sym)(at, s)]
    if z in "-+!":
      y += [col]
      if z == "!": klass = col
    elif z != "X": x += [col]
  return o(it=Cols, all=cols, x=x, y=y, names=names,
           klass=klass or y[0])

def Tree(): pass                    # interior tag (leaf=Data)
def Dim(): pass                     # projection-axis tag

# ## add / merge ------------------------------------------------
def add(i, v):
  if i.it is Data:
    if not i.cols: i.cols = Cols(v)
    else:
      i.rows += [v]
      for c in i.cols.all: add(c, v[c.at])
  elif v == "?": pass
  elif i.it is Sym:
    i.n += 1; i.has[v] = i.has.get(v, 0) + 1
  else:
    i.n += 1; d = v - i.mu
    i.mu += d / i.n
    i.m2 += d * (v - i.mu)
    i.sd = (i.m2/(i.n-1))**0.5 if i.n > 1 else 0
    if v < i.lo: i.lo = v
    if v > i.hi: i.hi = v
  return v

def adds(src, it=None):
  for x in iter(src): add((it := it or Num()), x)
  return it

def clone(root, rows):
  return adds(rows, adds([root.cols.names], Data()))

def merge(a, b, s=1):                # s=+1 add, s=-1 subtract
  if a.it is Sym:
    has = dict(a.has)
    for k, v in b.has.items(): has[k] = has.get(k, 0) + s*v
    has = {k: v for k, v in has.items() if v > 0}
    return o(it=Sym, at=a.at, txt=a.txt, n=a.n + s*b.n, has=has)
  n = a.n + s*b.n
  if n <= 0: return Num(a.at, a.txt)
  mu = (a.n*a.mu + s*b.n*b.mu) / n
  d  = b.mu - a.mu
  m2 = max(0, a.m2 + s*b.m2 + s*d*d*a.n*b.n / n)
  c  = Num(a.at, a.txt)
  c.n, c.mu, c.m2 = n, mu, m2
  c.lo, c.hi = min(a.lo, b.lo), max(a.hi, b.hi)
  return c

# ## metrics ----------------------------------------------------
def norm(c, v):
  return v if v == "?" else (v - c.lo) / (c.hi - c.lo + 1E-32)

def binOf(c, x, bins):               # key = cut value (rep edge for Num)
  if c.it is Sym: return x
  k = min(bins-1, int(bins*(x-c.lo)/(c.hi-c.lo+1E-32)))
  return c.lo + (k+1)*(c.hi-c.lo)/bins

def disty(d, r):
  s, n, p = 0, 0, the.p
  for c in d.cols.y:
    n += 1; s += abs(norm(c, r[c.at]) - c.goal)**p
  return (s/n)**(1/p) if n else 0

# ## dims (Fastmap projection -> bucket coords) -----------------
def _gap(c, a, b):              # Aha per-col distance, 0..1
  if a == "?" and b == "?": return 1
  if c.it is Sym: return 0 if a == b else 1
  a = norm(c, a) if a != "?" else None
  b = norm(c, b) if b != "?" else None
  if a is None: a = 0 if b > .5 else 1   # missing -> max gap
  if b is None: b = 0 if a > .5 else 1
  return abs(a - b)

def dist(data, r1, r2):         # Minkowski over x-cols, 0..1
  n = s = 0
  for c in data.cols.x:
    n += 1; s += _gap(c, r1[c.at], r2[c.at]) ** the.p
  return (s/n) ** (1/the.p) if n else 0

def _far(data, rows, r):        # row farthest from r
  return max(rows, key=lambda z: dist(data, z, r))

def proj(data, dim, r):         # position of r on dim's east-west axis
  a, b = dist(data, r, dim.east), dist(data, r, dim.west)
  return (a*a + dim.gap*dim.gap - b*b) / (2*dim.gap)

def perp(data, dim, r):         # perpendicular dist of r to the axis
  a = dist(data, r, dim.east)
  return max(0, a*a - proj(data, dim, r)**2) ** .5

def dimBin(data, dim, r):       # which of the.Bins divisions r lands in
  return max(0, min(the.Bins-1, int(the.Bins*proj(data,dim,r)/dim.gap)))

def dims(data):                 # -> (axes, buckets); buckets = rows
  rows, axes = data.rows, []    # grouped by their Dims-tuple of bin ids
  for _ in range(the.Dims):
    if not axes:                # 1st axis: far-far from random anchor
      east = _far(data, rows, random.choice(rows))
    else:                       # next: corner orthogonal to all axes
      east = max(rows, key=lambda z:
                 min(perp(data, a, z) for a in axes))
    west = _far(data, rows, east)
    axes.append(o(it=Dim, east=east, west=west,
                  gap=dist(data, east, west) or 1E-32))
  buckets = {}
  for r in rows:
    key = tuple(dimBin(data, a, r) for a in axes)
    buckets.setdefault(key, []).append(r)
  return axes, buckets

# ## tree (best cut over ALL cols) ------------------------------
def cutgo(c, x, v):                  # x -> left branch?
  return x != "?" and (x == v if c.it is Sym else x <= v)

def tree(root, stop=None, yfun=None, bins=None):
  bins = bins or the.Bins
  yfun = yfun or (lambda r: r[root.cols.klass.at])
  stop = stop or the.stop or len(root.rows)**.5

  def cuts(c, rows):                 # yield (impurity, REAL cut value)
    hist = {}                        # binkey -> [Num(yfun), max real x]
    for r in rows:
      if (x := r[c.at]) != "?":
        cell = hist.setdefault(binOf(c, x, bins), [Num(), x])
        add(cell[0], yfun(r))
        if c.it is not Sym and x > cell[1]: cell[1] = x
    bs = sorted(hist.items())
    if not bs: return
    total = reduce(merge, (cell[0] for _, cell in bs))
    if c.it is Sym:                  # cut = the category (already real)
      for key, (v, _) in bs:
        R = merge(total, v, -1)
        if v.n and R.n: yield v.m2 + R.m2, key
    else:                            # cut = largest real value going left
      L = Num()
      for _, (v, hi) in bs[:-1]:
        L = merge(L, v)
        R = merge(total, L, -1)
        if L.n and R.n: yield L.m2 + R.m2, hi

  def grow(rows):
    if len(rows) <= stop: return clone(root, rows)
    best, bc, bv = BIG, None, "?"      # best cut across ALL cols
    for c in root.cols.x:
      for imp, v in cuts(c, rows):
        if imp < best: best, bc, bv = imp, c, v
    if bc is None: return clone(root, rows)
    ok, no = [], []
    for r in rows:
      (ok if cutgo(bc, r[bc.at], bv) else no).append(r)
    if not (ok and no): return clone(root, rows)
    return o(it=Tree, at=bc.at, cut=bv, left=grow(ok), right=grow(no))

  return grow(root.rows)

def treeLeaf(root, t, row):          # route row to leaf
  while t.it is Tree:
    c = root.cols.all[t.at]
    t = t.left if cutgo(c, row[t.at], t.cut) else t.right
  return t

def leaves(t):                       # yield the leaf Datas
  if t.it is Data: yield t
  else: yield from leaves(t.left); yield from leaves(t.right)

def treeShow(root, t):               # ygoal mean + tree
  ys = [c for c in root.cols.y if c.it is Num]
  rowsOf = lambda t: t.rows if t.it is Data else \
                     rowsOf(t.left) + rowsOf(t.right)
  dy = lambda rs: adds(disty(root, r) for r in rs).mu
  hdr = "".join("%7s" % c.txt for c in ys)
  print("%6s%s %5s  tree" % ("ygoal", hdr, "n"))
  def show(t, lvl, txt):
    rs = rowsOf(t)
    g = "".join("%7.1f" %
                adds(r[c.at] for r in rs if r[c.at] != "?").mu
                for c in ys)
    print("%6.2f%s %5d  %s%s" %
          (dy(rs), g, len(rs), "|  "*lvl, txt))
    if t.it is Tree:
      c = root.cols.all[t.at]
      lo, hi = ("<=", " >") if c.it is Num else ("==", "!=")
      for kid, op in sorted([(t.left, lo), (t.right, hi)],
                            key=lambda k: dy(rowsOf(k[0]))):
        show(kid, lvl+1, "%s %s %s" % (c.txt, op, shrink(t.cut)))
  show(t, 0, "")

# ## config -----------------------------------------------------
def csv(file):
  for ln in open(file):
    ln = ln.strip()
    if ln and ln[0] != "#":
      yield [coerce(x.strip()) for x in ln.split(",")]

def coerce(z):
  for f in (int, float):
    try: return f(z)
    except: pass
  z = z.strip()
  return {'True': True, 'False': False, 'None': None}.get(z, z)

def settings(doc):
  return o(**{k: coerce(v)
              for k, v in re.findall(r'(\w+)=(\S+)', doc)})

def cli(the, doc, egs={}):
  flags = {f: l.lstrip("-")
           for s, l in re.findall(r"(-\w) (--\w+)", doc)
           for f in (s, l)}
  for j, s in enumerate(sys.argv):
    if fn := egs.get("test_" + s.lstrip("-")):
      random.seed(the.seed); fn()
    elif k := flags.get(s):
      v = the[k]
      if isinstance(v, bool): v = not v
      elif j+1 < len(sys.argv): v = coerce(sys.argv[j+1])
      the[k] = v
    elif re.match(r"-\D", s):
      sys.exit("bad flag: %s\n%s" % (s, doc))
  return the

# ## egs --------------------------------------------------------
def test_tree():                # tree on N rows of -f, show it
  head, *body = csv(the.file)
  rows = random.sample(body, min(the.N, len(body)))
  d = Data([head] + rows)
  treeShow(d, tree(d))

def test_dims():                # BINGO compression on full -f
  d = Data(list(csv(the.file)))
  _, buckets = dims(d)
  N, n = len(d.rows), len(buckets)
  sz = sorted((len(v) for v in buckets.values()), reverse=True)
  print("Dims=%d Bins=%d  rows=%d buckets=%d  r=n/N=%.3f"
        % (the.Dims, the.Bins, N, n, n/N))
  print("possible b^d=%d  occupied=%d  top sizes=%s"
        % (the.Bins**the.Dims, n, sz[:10]))

# ## main -------------------------------------------------------
the = settings(__doc__)
random.seed(the.seed)
if __name__ == "__main__":
  cli(the, __doc__, globals())
