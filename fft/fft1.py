#!/usr/bin/env python3 -B
"""
fft1.py, random-cluster forest engine + confusion (library)
(c) 2025, Tim Menzies <timm@ieee.org>, MIT license

Generic services: Data, distx/disty, rmap (random 1-pole radial
cluster trees), leaf (router), confused (pd/pf/.. from (want,got)
pairs), cli/the. No task knowledge here -- see e.g. compas.py.

Options:
 -s --seed  random seed       seed=1234567891
 -p --p     distance exponent p=2
 -R --Round repr decimals     Round=2
 -b --Bins  numeric bins      Bins=5
 -S --stop  leaf size         stop=None
 -n --N     demo row sample   N=50
 -f --file  data file         file=/Users/timm/gits/moot/optimize/misc/auto93.csv

eg: python3 fft1.py -f FILE -n 50 --tree
"""
import random, math, sys, re
BIG = math.inf

# ## constructors -----------------------------------------------
def Num(at=0, txt=""):
  return o(it=Num, at=at, txt=txt, n=0, mu=0, m2=0, sd=0,
           lo=BIG, hi=-BIG, goal=0 if txt.endswith("-") else 1)

def Sym(at=0, txt=""):
  return o(it=Sym, at=at, txt=txt, n=0, has={})

def Data(src=[]):
  return adds(src, o(it=Data, cols=None, rows=[]))

def Cols(names):
  cols,x,y,klass = [],[],[],None
  for at, s in enumerate(names):
    z = s[-1]
    cols += [col := (Num if s[0].isupper() else Sym)(at, s)]
    if z in "-+!":
      y += [col]      
      if z == "!": klass = col
    elif z != "X":  x += [col]   
  return o(
    it=Cols, all=cols, x=x,y=y, names=names, klass=klass or y[0])

def Tree(): pass
def Rtree(): pass

# ## add (one polymorphic) --------------------------------------
def add(i, v):
  if i.it is Data:
    if not i.cols: i.cols = Cols(v)
    else:
      i.rows += [v]
      for c in i.cols.all: add(c, v[c.at])
  elif v == "?": pass
  elif i.it is Sym:
    i.n += 1; i.has[v] = i.has.get(v, 0) + 1
  elif isinstance(v, dict):            # combine another Num into i
    if v.n:
      n = i.n + v.n; d = v.mu - i.mu
      i.m2 += v.m2 + d*d * i.n*v.n/n
      i.mu  = (i.n*i.mu + v.n*v.mu) / n
      i.n, i.lo, i.hi = n, min(i.lo, v.lo), max(i.hi, v.hi)
      i.sd  = (i.m2/(i.n-1))**0.5 if i.n > 1 else 0
  else:                                # add a number
    i.n += 1; d = v - i.mu
    i.mu += d / i.n
    i.m2 += d * (v - i.mu)
    i.sd = (i.m2/(i.n-1))**0.5 if i.n > 1 else 0
    if v < i.lo: i.lo = v
    if v > i.hi: i.hi = v
  return v

def adds(src, it=None):
  it = it or Num(); [add(it, x) for x in iter(src)]; return it

def clone(root, rows):
  return adds(rows, adds([root.cols.names],Data()))

# ## metrics ----------------------------------------------------
def norm(c, v): 
  return v if v=="?" else (v - c.lo) / (c.hi - c.lo + 1E-32)

def bucket(c, v): 
  if v == "?" or c.it is Sym: return v
  return min(the.Bins-1, int(the.Bins*norm(c, v)))

def goes(c, v, b): 
  x = bucket(c, v)  
  return x !="?" and (x == b if c.it is Sym else x <= b)

def distx(d, r1, r2):
  s, cols, p = 0, d.cols.x, the.p
  for c in cols:
    a, b = r1[c.at], r2[c.at]
    if c.it is Sym: s += a != b
    elif a == "?" and b == "?": s += 1
    else:
      a = norm(c, a); b = norm(c, b)
      if a == "?": a = 0 if b > .5 else 1
      if b == "?": b = 0 if a > .5 else 1
      s += abs(a - b) ** p
  return (s / len(cols)) ** (1/p)

def disty(d, r):
  s, n, p = 0, 0, the.p
  for c in d.cols.y:
    if c.it is Num and r[c.at] != "?":
      n += 1; s += abs(norm(c, r[c.at]) - c.goal)**p
  return (s/n)**(1/p) if n else 0

# ## rmap (random 1-pole radial cluster tree) -------------------
def rtree(root, stop=None):
  def grow(rows, stop):
    if len(rows) <= stop: return clone(root, rows)
    lo  = random.choice(rows)               # random pole
    rows.sort(key=lambda r: distx(root,r,lo)) # near->far
    m   = len(rows) // 2
    cut = distx(root, rows[m], lo)              # split radius
    return o(it=Rtree, lo=lo, cut=cut,
             left  = grow(rows[:m], stop),       # dist <= cut
             right = grow(rows[m:], stop))
  return grow(root.rows, stop or the.stop or len(root.rows)**.5)

def rtreeLeaf(root, tree, row):  # route a new row down the tree
  while tree.it is Rtree:
    tree = (tree.left if distx(root, row, tree.lo) <= tree.cut
                      else tree.right)
  return tree

# ## tree (supervised: split to minimise y-sd) -------------
def tree(data, rows=None):  # regression tree on disty(row)
  m = the.stop or 4
  def grow(rows):
    t = o(it=Tree, root=data, at=None, bin=None, kids={},
          n=len(rows), ymu=adds(disty(data,r) for r in rows).mu,
          ys=[adds(r[c.at] for r in rows if r[c.at]!="?")
              for c in data.cols.y])
    if len(rows) > 2*m:
      for _, at, b in treeCuts(data, rows):   # best-first
        c, ok, no = data.cols.all[at], [], []
        for r in rows:
          (ok if goes(c, r[c.at], b) else no).append(r)
        if len(ok) >= m and len(no) >= m:
          t.at, t.bin = at, b
          kids = {True: grow(ok), False: grow(no)}    # best-mu first
          t.kids = dict(sorted(kids.items(), key=lambda kv: kv[1].ymu))
          break
    return t
  return grow(rows or data.rows)

def treeCuts(data, rows):
  d = {}
  for r in rows:
    y = disty(data, r)
    for c in data.cols.x:
      k = (c.at, bucket(c, r[c.at]))
      if k[1] != "?":
        if k not in d: d[k] = Num()
        add(d[k], y)
  return sorted(cut for c in data.cols.x for cut in treeCut(d, c))

def treeCut(d, col):               # yield (exp-sd, at, bin)
  bins = sorted((b, n) for (at,b),n in d.items() if at==col.at)
  if len(bins) < 2: return
  xp = lambda a, b: (a.n*a.sd + b.n*b.sd) / (a.n + b.n)
  for j, (b, num) in enumerate(bins):
    if col.it is Sym:              # == b  vs  rest
      rest = adds(n for k,(_,n) in enumerate(bins) if k != j)
      yield xp(num, rest), col.at, b
    elif j < len(bins)-1:          # <= bin b  vs  >
      yield xp(adds(n for _,n in bins[:j+1]),
               adds(n for _,n in bins[j+1:])), col.at, b

def treeLeaf(tree, row):              # route a new row down the tree
  while tree.kids:
    c = tree.root.cols.all[tree.at]
    tree = tree.kids[goes(c, row[tree.at], tree.bin)]
  return tree

def treeShow(root):                   # ygoal, per-goal means, then tree
  ys = "".join("%7s" % c.txt for c in root.root.cols.y)
  print("%6s%s %5s  tree" % ("ygoal", ys, "n"))
  def show(t, lvl, txt):
    g = "".join("%7.1f" % y.mu for y in t.ys)
    print("%6.2f%s %5d  %s%s" % (t.ymu, g, t.n, "|  "*lvl, txt))
    if t.kids:
      c = root.root.cols.all[t.at]
      for k in t.kids:                       # already best-mu first
        op = ("==" if k else "!=") if c.it is Sym else \
             ("<=" if k else " >")
        show(t.kids[k], lvl+1, "%s %s %s" % (c.txt, op, t.bin))
  show(root, 0, "")

# ## confusion (pd, pf, ... from (want,got) pairs) --------------
def confused(pairs):            # pairs = [(want, got), ...]
  n, labels, N = {}, set(), len(pairs)
  for wg in pairs:
    n[wg] = n.get(wg, 0) + 1
    labels |= set(wg)
  out = {}
  for k in labels:                       # one-vs-rest per label
    tp = n.get((k, k), 0)
    fn = sum(c for (w,g),c in n.items() if w==k and g!=k)
    fp = sum(c for (w,g),c in n.items() if w!=k and g==k)
    tn = N - tp - fn - fp
    pd, pf = tp/(tp+fn+1e-32), fp/(fp+tn+1e-32)    # recall, fpr
    prec   = tp/(tp+fp+1e-32)
    out[k] = o(it=o, label=k, tp=tp, fp=fp, fn=fn, tn=tn,
               pd=pd, pf=pf, prec=prec, acc=(tp+tn)/(N+1e-32),
               f1=2*prec*pd/(prec+pd+1e-32))
  return out

# ## lib -------------------------------------------------------
class o(dict):  
  __getattr__ = dict.get;__setattr__ = dict.__setitem__;
  def __repr__(i):
    def f(v):
      if isinstance(v,float):
        return int(v) if v==int(v) else round(v, the.Round)
      return v
    return "{" + " ".join(":%s %s" % (k, f(i[k])) for k in i
                          if str(k)[0]!="_") + "}"

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
  return {'True':True,'False':False,'None':None}.get(z,z)

def settings(doc):              # key=val defaults from doc
  return o(**{k: coerce(v)
              for k, v in re.findall(r'(\w+)=(\S+)', doc)})

def cli(the, doc, egs={}):      # update `the`; --x runs test_x(); flags from doc
  flags = {f: l.lstrip("-")
           for s,l in re.findall(r"(-\w) (--\w+)",doc)
           for f in (s,l)}
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

# ## egs (run via --name) --------------------------------------
def test_the():                 # show the config
  print(the)

def test_tree():                # tree on N random rows of -f file
  head, *body = csv(the.file)
  rows = random.sample(body, min(the.N, len(body)))
  treeShow(tree(Data([head] + rows)))

# ## main -------------------------------------------------------
the = settings(__doc__)
random.seed(the.seed)
if __name__ == "__main__":
  cli(the, __doc__, globals())
