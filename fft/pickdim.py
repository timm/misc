#!/usr/bin/env python3 -B
"""
pickdim.py (v2, cell-based), honest train/select/test via BINGO
cells. Per rep, 60/20/20; repeat Projs(N) times: project train into
~80 occupied cells (one pre-summed label Num each), build Models(M)
cell-trees over them (ratio-sweep = REWEIGHT cells, no resampling);
score each on val, best -> test -> ONE dot. Trees are ~free; cost
is the N projections. fit projected once (reuse dims buckets); val
projected once per projection (reused across the M trees).

Options:
 -s --seed   random seed     seed=1234567891
 -p --p      distance exp    p=2
 -R --Round  repr decimals   Round=2
 -d --Dims   projection dims Dims=5
 -b --Bins   divs per dim    Bins=5
 -F --few    pole sample     few=32
 -P --Projs  projections/rep Projs=10
 -M --Models trees/projection Models=80
 -r --reps   repeats         reps=20
 -n --N      total rows (60/20/20) N=2000
 -v --Val    max val rows    Val=200
 -g --group  protected col   group=race
 -f --file   data file       file=/Users/timm/gits/moot/classify/COMPAS53.csv
"""
import random, os, math
from collections import Counter
from functools import reduce
import tree
from tree import o, Num, Data, clone, csv, dims, dimBin, merge

the = tree.settings(__doc__); tree.the = the
d = lab = grp = groups = None              # set in load() after cli

def load():
  global d, lab, grp, groups
  d   = Data(list(csv(the.file)))
  yat = next(c.at for c in d.cols.all if str(c.txt).endswith("!"))
  gat = next(c.at for c in d.cols.all if c.txt == the.group)
  lab = {id(r): (0 if r[yat] in ("?", "") else int(r[yat])) for r in d.rows}
  grp = {id(r): r[gat] for r in d.rows}
  groups = [g for g, _ in Counter(grp.values()).most_common(2)]

def confused(pairs):
  n, labels, N = {}, set(), len(pairs)
  for wg in pairs:
    n[wg] = n.get(wg, 0) + 1; labels |= set(wg)
  out = {}
  for k in labels:
    tp = n.get((k, k), 0)
    fn = sum(c for (w, g), c in n.items() if w == k and g != k)
    fp = sum(c for (w, g), c in n.items() if w != k and g == k)
    tn = N - tp - fn - fp
    out[k] = o(pd=tp/(tp+fn+1e-32), pf=fp/(fp+tn+1e-32),
               prec=tp/(tp+fp+1e-32))
  return out

def heaven(m): return math.sqrt(sum((1-v)**2 for v in m))

# ## cells ------------------------------------------------------
def cellFrom(co, rows):                    # rows of one cell -> summary
  c = o(co=co, pos=0, neg=0, g={gg: [0, 0] for gg in groups})
  for r in rows:
    y = lab[id(r)]
    if y: c.pos += 1
    else: c.neg += 1
    gg = grp[id(r)]
    if gg in groups: c.g[gg][0 if y else 1] += 1
  return c

def labNum(c, t):                          # cell -> ratio-weighted label Num
  p, ne = c.pos*t, c.neg; n = p + ne
  nm = Num()
  if n: nm.n, nm.mu, nm.m2 = n, p/n, n*(p/n)*(1-p/n)
  return nm

def celltree(cells, ndim, t, stop):        # tree over cells, cut on coord dims
  def cuts(j, cs):
    g = {}
    for c in cs:
      k = c.co[j]
      g[k] = merge(g[k], labNum(c, t)) if k in g else labNum(c, t)
    bs = sorted(g.items())
    if len(bs) < 2: return
    tot = reduce(merge, (v for _, v in bs))
    L = Num()
    for v, nm in bs[:-1]:
      L = merge(L, nm); R = merge(tot, L, -1)
      if L.n and R.n: yield L.m2 + R.m2, v
  def grow(cs):
    if len(cs) <= stop: return cs          # leaf = list of cells
    best, bj, bv = 1e18, None, None
    for j in range(ndim):
      for imp, v in cuts(j, cs):
        if imp < best: best, bj, bv = imp, j, v
    if bj is None: return cs
    ok = [c for c in cs if c.co[bj] <= bv]
    no = [c for c in cs if c.co[bj] >  bv]
    return o(it="T", j=bj, cut=bv, left=grow(ok), right=grow(no))
  return grow(cells)

def route(t, co):                          # -> leaf (list of cells)
  while isinstance(t, o): t = t.left if co[t.j] <= t.cut else t.right
  return t

def vote(leaf, t):                         # ratio-weighted majority
  p = sum(c.pos for c in leaf)*t; ne = sum(c.neg for c in leaf)
  return int(p > ne)

def coordsOf(ftrain, axes, rows):          # rows -> (coord, label, group)
  return [(tuple(dimBin(ftrain, a, r) for a in axes),
           lab[id(r)], grp[id(r)]) for r in rows]

def score(coco, t_, t):                    # -> (recall, prec, fair)
  pairs = []; gp = {g: [] for g in groups}
  for co, y, gg in coco:
    got = vote(route(t_, co), t)
    pairs.append((y, got))
    if gg in groups: gp[gg].append((y, got))
  c   = confused(pairs).get(1, o(pd=0, prec=0))
  fpr = [confused(x).get(1, o(pf=0)).pf for x in gp.values()]
  return c.pd, c.prec, min(fpr)/(max(fpr)+1e-32)

if __name__ == "__main__":
  tree.cli(the, __doc__)
  load(); random.seed(the.seed)
  rows = d.rows[:]
  if len(rows) > the.N: rows = random.sample(rows, the.N)
  name = "%s / %s" % (os.path.basename(the.file).split(".")[0], the.group)
  cloud, te_rows = [], []
  for i in range(the.reps):
    random.shuffle(rows)
    n1, n2 = int(.6*len(rows)), int(.8*len(rows))
    fit, val, test = rows[:n1], rows[n1:n2], rows[n2:]
    if len(val) > the.Val: val = random.sample(val, the.Val)
    ftrain = clone(d, fit)
    best, bestd = None, 1e9
    for _ in range(the.Projs):                       # N projections
      axes, buckets = dims(ftrain)                   # fit projected once
      cells = [cellFrom(co, rs) for co, rs in buckets.items()]
      P  = sum(c.pos for c in cells); Ne = sum(c.neg for c in cells)
      if not P or not Ne: continue
      stop = max(2, round(len(cells)**.5))
      valco = coordsOf(ftrain, axes, val)            # val projected once
      for _ in range(the.Models):                    # M cell-trees (free)
        x  = random.randint(10, 90)
        t  = (x/(100-x)) * (Ne/(P+1e-9))             # ratio reweight
        tr = celltree(cells, len(axes), t, stop)
        mv = score(valco, tr, t)
        if i == 0: cloud.append(mv)
        if heaven(mv) < bestd: bestd, best = heaven(mv), (axes, tr, t)
    axes, tr, t = best
    te_rows.append(score(coordsOf(ftrain, axes, test), tr, t))
  print("#", name)
  for r, p, f in cloud:   print("CLOUD %.4f %.4f %.4f" % (r, f, p))
  for r, p, f in te_rows: print("TEST  %.4f %.4f %.4f" % (r, f, p))
