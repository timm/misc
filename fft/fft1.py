#!/usr/bin/env python3 -B
"""
fft1.py, fastmap forest engine + confusion metrics (library)
(c) 2025, Tim Menzies <timm@ieee.org>, MIT license

Generic services: Data, distx/disty, rmap (2-pole cosine-proj
trees), leaf (router), confused (pd/pf/.. from (want,got) pairs),
cli/the. No task knowledge here -- see e.g. compas.py.

Options:
 -s --seed  random seed       seed=1234567891
 -p --p     distance exponent p=2
 -R --Round repr decimals     Round=2
 -S --stop  leaf size         stop=None
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
  return adds(src, o(it=Data, cols=None, rows=[], p=the.p))  # p travels w/ model

def Cols(names):
  cols,x,y = [],[],[]
  for at, s in enumerate(names):
    z = s[-1]
    cols += [col := (Num if z.isupper() else Sym)(at, s)]
    if   z in "-+!": y += [col]
    elif z != "X"  : x += [col]
  return  o(it=Cols, all=cols, x=x, y=y, names=names)

# ## add (one polymorphic) --------------------------------------
def add(i, v):
  if i.it is Data:
    if not i.cols: i.cols = Cols(v)
    else:
      i.rows += [v]
      for c in i.cols.all: add(c, v[c.at])
  elif v != "?":
    i.n += 1
    if i.it is Sym: i.has[v] = i.has.get(v, 0) + 1
    else:
      d = v - i.mu
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

def distx(d, r1, r2):
  s, cols, p = 0, d.cols.x, d.p          # feature-subset + p travel with d
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
  s, n, p = 0, 0, d.p
  for c in d.cols.y: n += 1; s += abs(norm(c, r[c.at]) - c.goal)**p
  return (s/n)**(1/p) if n else 0

# ## fastmap ----------------------------------------------------
def far(root, rows, ref):       # the 0.9-quantile distant point
  rows.sort(key=lambda r: distx(root, ref, r))
  return rows[int(0.9 * len(rows))]

def project(root, r, a, b, c):  # cosine-rule proj of r onto line a-b
  return (distx(root,r,a)**2 + c*c - distx(root,r,b)**2) / (2*c)

def rmap(root, stop=None):
  def grow(rows, stop):
    if len(rows) <= stop: return clone(root, rows)
    a   = far(root, rows, random.choice(rows))  # pole a (far from random)
    b   = far(root, rows, a)                    # pole b (far from a)
    c   = distx(root, a, b) + 1e-32
    rows.sort(key=lambda r: project(root, r, a, b, c))
    m   = random.randint(1, len(rows)-1)        # random cut between poles
    cut = project(root, rows[m], a, b, c)
    return o(it=Node, a=a, b=b, c=c, cut=cut,
             left  = grow(rows[:m], stop),       # proj <= cut
             right = grow(rows[m:], stop))
  return grow(root.rows, stop or the.stop or len(root.rows)**.5)

def Node(): pass

def leaf(root, tree, row):  # route new item down tree
  while tree.it is Node:
    x = project(root, row, tree.a, tree.b, tree.c)
    tree = tree.left if x <= tree.cut else tree.right
  return tree

# faster variant: ONE random pole, split by distance to it ------
def rmapf(root, stop=None):
  def grow(rows, stop):
    if len(rows) <= stop: return clone(root, rows)
    lo  = random.choice(rows)                   # single random pole
    rows.sort(key=lambda r: distx(root, r, lo)) # near -> far from lo
    m   = len(rows) // 2
    cut = distx(root, rows[m], lo)              # split radius
    return o(it=Node, lo=lo, cut=cut,
             left  = grow(rows[:m], stop),       # dist <= cut
             right = grow(rows[m:], stop))
  return grow(root.rows, stop or the.stop or len(root.rows)**.5)

def leaff(root, tree, row):  # router for rmapf trees
  while tree.it is Node:
    tree = (tree.left if distx(root, row, tree.lo) <= tree.cut
                      else tree.right)
  return tree

# ## confusion (pd, pf, ... from (want,got) pairs) --------------
def confused(pairs):            # pairs = [(want, got), ...]
  m, labels, N = {}, set(), len(pairs)
  for w, g in pairs:
    m.setdefault(w, {})[g] = m.setdefault(w, {}).get(g, 0) + 1
    labels |= {w, g}
  out = {}
  for k in labels:
    tp = m.get(k, {}).get(k, 0)
    fn = sum(v for g, v in m.get(k, {}).items() if g != k)
    fp = sum(m.get(w, {}).get(k, 0) for w in labels if w != k)
    tn = N - tp - fn - fp
    pd   = tp / (tp + fn + 1e-32)        # recall / true positive rate
    pf   = fp / (fp + tn + 1e-32)        # false alarm rate
    prec = tp / (tp + fp + 1e-32)
    out[k] = o(it=o, label=k, tp=tp, fp=fp, fn=fn, tn=tn,
               pd=pd, pf=pf, prec=prec, acc=(tp + tn) / (N + 1e-32),
               f1=2 * prec * pd / (prec + pd + 1e-32))
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

def settings(doc):              # parse "key=val" defaults from a docstring
  return o(**{k: coerce(v) for k, v in re.findall(r"(\w+)=(\S+)", doc)})

def cli(the, doc):              # update `the` from sys.argv, flags from doc
  flags = {f: l.lstrip("-")
             for s, l in re.findall(r"(-\w) (--\w+)", doc)
             for f in (s, l)}
  for j, s in enumerate(sys.argv):
    if k := flags.get(s):
      v = the[k]
      if isinstance(v, bool): v = not v
      elif j+1 < len(sys.argv): v = coerce(sys.argv[j+1])
      the[k] = v
    elif re.match(r"-\D", s):
      sys.exit("bad flag: %s\n%s" % (s, doc))
  return the

# ## main -------------------------------------------------------
the = settings(__doc__)

if __name__ == "__main__":
  print(cli(the, __doc__))
