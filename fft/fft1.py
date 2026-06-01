#!/usr/bin/env python3 -B
"""
fft1.py, random-cluster forest engine + confusion (library)
(c) 2025, Tim Menzies <timm@ieee.org>, MIT license

Generic services: Data, disty, tree (random axis-cut cluster),
treeLeaf (router), treeShow (printer), confused (pd/pf/.. from
(want,got) pairs), cli/the. No task knowledge -- see compas.py.

Options:
 -s --seed  random seed       seed=1234567891
 -p --p     distance exponent p=2
 -R --Round repr decimals     Round=2
 -S --stop  leaf size         stop=None
 -n --N     demo row sample   N=50
 -f --file  data file
            file=/Users/timm/gits/moot/optimize/misc/auto93.csv

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

def Tree(): pass                    # interior tag (leaf=Data)

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
  else:                                # add a number
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
  return adds(rows, adds([root.cols.names],Data()))

# ## metrics ----------------------------------------------------
def norm(c, v): 
  return v if v=="?" else (v - c.lo) / (c.hi - c.lo + 1E-32)

def disty(d, r):
  s, n, p = 0, 0, the.p
  for c in d.cols.y:
    n += 1; s += abs(norm(c, r[c.at]) - c.goal)**p
  return (s/n)**(1/p) if n else 0

# ## tree (random axis-cut cluster tree) -----------------------
def cutgo(c, x, v):                  # x -> left branch?
  return x != "?" and (x == v if c.it is Sym else x <= v)

def tree(root, stop=None):
  cols = root.cols.x
  stop = stop or the.stop or len(root.rows)**.5
  def grow(rows):
    if len(rows) <= stop: return clone(root, rows)
    for _ in range(10):              # random col + cut
      c = random.choice(cols)
      v = random.choice(rows)[c.at]
      if v == "?": continue
      ok, no = [], []
      for r in rows:
        (ok if cutgo(c,r[c.at],v) else no).append(r)
      if ok and no: break
    else:
      return clone(root, rows)       # no good cut -> leaf
    return o(it=Tree, at=c.at, cut=v,
             left=grow(ok), right=grow(no))
  return grow(root.rows)

def treeLeaf(root, t, row):          # route row to leaf
  while t.it is Tree:
    c = root.cols.all[t.at]
    t = t.left if cutgo(c, row[t.at], t.cut) else t.right
  return t

def treeShow(root, t):               # ygoal, goal means, tree
  ys  = [c for c in root.cols.y if c.it is Num]
  rowsOf = lambda t: t.rows if t.it is Data else \
                     rowsOf(t.left) + rowsOf(t.right)
  dy  = lambda rs: adds(disty(root,r) for r in rs).mu
  hdr = "".join("%7s" % c.txt for c in ys)
  print("%6s%s %5s  tree" % ("ygoal", hdr, "n"))
  def show(t, lvl, txt):
    rs = rowsOf(t)
    g = "".join("%7.1f" %
                adds(r[c.at] for r in rs if r[c.at]!="?").mu
                for c in ys)
    print("%6.2f%s %5d  %s%s" %
          (dy(rs), g, len(rs), "|  "*lvl, txt))
    if t.it is Tree:
      c  = root.cols.all[t.at]
      lo, hi = ("<="," >") if c.it is Num else ("==","!=")
      for kid, op in sorted([(t.left, lo), (t.right, hi)],
                       key=lambda k: dy(rowsOf(k[0]))):
        show(kid, lvl+1, "%s %s %s" % (c.txt, op, t.cut))
  show(t, 0, "")

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

def cli(the, doc, egs={}):      # --x runs test_x()
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

def test_tree():                # tree on N rows of -f
  head, *body = csv(the.file)
  rows = random.sample(body, min(the.N, len(body)))
  d = Data([head] + rows)
  treeShow(d, tree(d))

# ## main -------------------------------------------------------
the = settings(__doc__)
random.seed(the.seed)
if __name__ == "__main__":
  cli(the, __doc__, globals())
