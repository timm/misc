#!/usr/bin/env python3 -B
"""
fft1.py, fastmap forest -> (acc, fairness) model cloud
(c) 2025, Tim Menzies <timm@ieee.org>, MIT license

stdout: one line per model = "acc fairness".
repeats x trees models: each shuffle splits 80/20, learns
trees on train, scores each tree on the 20% holdout.

Options:
 -s --seed    random seed   seed=1234567891
 -t --trees   trees/shuffle trees=100
 -r --repeats shuffles      repeats=20
 -n --N       row subsample  N=1000
 -S --stop    leaf size      stop=None
 -f --file    data file     file=/Users/timm/gits/moot/classify/COMPAS53.csv
"""
import random, math, sys, re
from types import SimpleNamespace as o

BIG = math.inf

def coerce(z):
  for f in (int, float):
    try: return f(z)
    except: pass
  z = z.strip()
  return {'True':True,'False':False,'None':None}.get(z,z)

help = re.findall(r"(\w+)=(\S+)", __doc__)
the  = o(**{k: coerce(v) for k, v in help})

def cli(the):
  ab = re.findall(r"-(\w) --(\w+)", __doc__)
  flags = {"-"+c: k for c, k in ab}
  flags.update({"--"+k: k for k in vars(the)})
  for i, s in enumerate(sys.argv):
    if (k := flags.get(s)) and i+1 < len(sys.argv):
      setattr(the, k, coerce(sys.argv[i+1]))
  return the

# ## constructors -----------------------------------------------
def Num(at=0, txt=""):
  return o(it=Num, at=at, txt=txt, n=0, mu=0, m2=0, sd=0,
           lo=BIG, hi=-BIG, heaven=0 if txt.endswith("-") else 1)

def Sym(at=0, txt=""):
  return o(it=Sym, at=at, txt=txt, n=0, has={})

def Data(src=[]):
  d = o(it=Data, cols=None, rows=[])
  return adds(src,d)

# ## add (one polymorphic) --------------------------------------
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

def csv(file):
  for ln in open(file):
    ln = ln.strip()
    if ln and ln[0] != "#":
      yield [coerce(x.strip()) for x in ln.split(",")]

def adds(src, it=None):
  it = it or Num(); [add(it, x) for x in iter(src)]; return it

def clone(root, rows):
  return adds(rows, adds([root.cols.names],Data()))

# ## metrics ----------------------------------------------------
def norm(c, v): 
  return v if v=="?" else (v - c.lo) / (c.hi - c.lo + 1E-32)

def distx(d, r1, r2):
  s, cols = 0, d.cols.x
  for c in cols:
    a, b = r1[c.at], r2[c.at]
    if c.it is Sym: s += a != b
    elif a == "?" and b == "?": s += 1
    else:
      a = norm(c, a); b = norm(c, b)
      if a == "?": a = 0 if b > .5 else 1
      if b == "?": b = 0 if a > .5 else 1
      s += (a - b) ** 2
  return (s / len(cols)) ** 0.5

def disty(d, r):
  s, n = 0, 0
  for c in d.cols.y:
    if c.it is Num and r[c.at] != "?":
      n += 1; s += (norm(c, r[c.at]) - c.heaven)**2
  return (s/n)**0.5 if n else 0

# ## fastmap ----------------------------------------------------
def far(d, rs, ref):
  rs.sort(key=lambda r: distx(d, ref, r))
  return rs[int(0.9 * len(rs))]

def fastmap(root, stop=None):
  stop = stop or the.stop or len(root.rows)**.5
  def grow(sub):
    if len(sub.rows) <= stop: return sub  # leaf = Data
    lo = far(root, sub.rows[:], random.choice(sub.rows))  # pole
    sub.rows.sort(key=lambda r: distx(root, r, lo))  # near->far
    m   = len(sub.rows) // 2
    cut = distx(root, sub.rows[m], lo)  # split radius
    return o(it=Node, lo=lo, cut=cut,
             left  = grow(clone(root, sub.rows[:m])),  # <=cut
             right = grow(clone(root, sub.rows[m:])))
  return grow(root)

def Node(): pass

def leaf(root, tree, row):  # route new item down tree
  while tree.it is Node:
    tree = (tree.left if distx(root, row, tree.lo) <= tree.cut
                      else tree.right)
  return tree

# ## fairness experiment (COMPAS) -------------------------------
# pre-decision features (Upper=numeric, lower=symbolic).
FEATS = [("Age","Age"), ("Priors_count","Priors"),
         ("Juv_fel_count","JuvFel"), ("Juv_misd_count","JuvMisd"),
         ("Juv_other_count","JuvOther"), ("sex","sex"),
         ("race","race"), ("c_charge_degree","charge")]
LABEL, GROUP = "two_year_recid!", "race"

def features():               # -> rows(feature lists), lab, grp
  raw = list(csv(the.file)); head = raw[0]
  ix  = {n: i for i, n in enumerate(head)}
  names, rows, lab, grp = [nm for _, nm in FEATS], [], {}, {}
  for r in raw[1:]:
    fr = [r[ix[c]] for c, _ in FEATS]
    rows.append(fr)
    lab[id(fr)] = 0 if r[ix[LABEL]] in ("?","") else int(r[ix[LABEL]])
    grp[id(fr)] = r[ix[GROUP]]
  return names, rows, lab, grp

def evaluate(train, tree, test, lab, grp, groups):
  tp = tn = fp = fn = 0                         # accuracy counts
  gfp = {g:0 for g in groups}; gtn = {g:0 for g in groups}
  for r in test:
    L = leaf(train, tree, r)                    # leaf of train rows
    p = sum(lab[id(x)] for x in L.rows)/len(L.rows) >= .5  # leaf vote
    y = lab[id(r)]; g = grp[id(r)]
    if   y and p: tp += 1
    elif y:       fn += 1
    elif p:       fp += 1
    else:         tn += 1
    if not y and g in groups:                   # FPR per group
      gfp[g] += p; gtn[g] += not p
  acc  = (tp+tn)/(tp+tn+fp+fn+1e-32)
  fpr  = [gfp[g]/(gfp[g]+gtn[g]+1e-32) for g in groups]
  fair = min(fpr)/(max(fpr)+1e-32)              # predictive equality
  return acc, fair

# ## main -------------------------------------------------------
if __name__ == "__main__":
  cli(the)
  random.seed(the.seed)
  from collections import Counter
  names, rows, lab, grp = features()
  if len(rows) > the.N: rows = random.sample(rows, the.N)
  groups = [g for g,_ in Counter(grp.values()).most_common(2)]
  for _ in range(the.repeats):
    random.shuffle(rows)
    cut   = int(.8 * len(rows))
    train = Data([names] + rows[:cut]); test = rows[cut:]
    for _ in range(the.trees):
      acc, fair = evaluate(train, fastmap(train), test, lab, grp, groups)
      print("%.4f %.4f" % (acc, fair))
