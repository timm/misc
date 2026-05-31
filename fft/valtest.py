#!/usr/bin/env python3 -B
"""
valtest.py, can val-score predict test-score? (selection sanity)
build M cheap models on fit, score each on val AND test (heaven to
1,1,1). pearson r(val,test). r~0 => selection is guessing; best-on
-val regresses on test => scattered test dots. r high => cloud has
real per-model signal and scatter is just small-sample.

Options:
 -s --seed seed seed=1234567891
 -m --models models models=400
 -n --N rows N=1000
 -p --p exp p=2
"""
import random, math
from collections import Counter
import fft1
from fft1 import o, Data, clone, tree, treeLeaf, confused, csv

the = fft1.settings(__doc__); fft1.the = the
H = "/Users/timm/tmp/"
SETS = [("compas", H+"compas.csv", "race"),
        ("adult",  H+"adult.csv",  "race"),
        ("dutch",  H+"dutchf.csv", "sex"),
        ("diab",   H+"diab.csv",   "race")]

def leaves(t):
  if t.it is fft1.Data: yield t
  else: yield from leaves(t.left); yield from leaves(t.right)

def setVotes(t, lab):
  for lf in leaves(t):
    s = sum(lab[id(r)] for r in lf.rows)
    lf.vote = int(2*s > len(lf.rows))

def H3(m, t, rows, lab, grp, groups):       # heaven on a rowset
  pairs = []; gp = {g: [] for g in groups}
  for r in rows:
    got = treeLeaf(m, t, r).vote
    pairs.append((lab[id(r)], got))
    if grp[id(r)] in groups: gp[grp[id(r)]].append((lab[id(r)], got))
  c   = confused(pairs).get(1, o(pd=0, prec=0))
  fpr = [confused(x).get(1, o(pf=0)).pf for x in gp.values()]
  rec, prec = c.pd, c.prec
  fair = min(fpr)/(max(fpr)+1e-32)
  return math.sqrt((1-rec)**2 + (1-prec)**2 + (1-fair)**2)

def pearson(xs, ys):
  n = len(xs); mx = sum(xs)/n; my = sum(ys)/n
  cov = sum((a-mx)*(b-my) for a, b in zip(xs, ys))
  sx  = sum((a-mx)**2 for a in xs)**.5
  sy  = sum((b-my)**2 for b in ys)**.5
  return cov/(sx*sy+1e-32)

def run(name, file, gattr):
  d = Data(list(csv(file)))
  yat = next(c.at for c in d.cols.all if str(c.txt).endswith("!"))
  gat = next(c.at for c in d.cols.all if c.txt == gattr)
  lab = {id(r): (0 if r[yat] in ("?", "") else int(r[yat]))
         for r in d.rows}
  grp = {id(r): r[gat] for r in d.rows}
  groups = [g for g, _ in Counter(grp.values()).most_common(2)]
  rows = d.rows[:]
  if len(rows) > the.N: rows = random.sample(rows, the.N)
  random.shuffle(rows)
  n1, n2 = int(.6*len(rows)), int(.8*len(rows))
  fit, val, test = rows[:n1], rows[n1:n2], rows[n2:]
  base = clone(d, fit)
  pos = [r for r in fit if lab[id(r)] == 1]
  neg = [r for r in fit if lab[id(r)] == 0]
  vh, th = [], []
  for _ in range(the.models):
    x   = random.randint(10, 90)
    nps = min(len(pos), round(64*x/100))
    bag = random.sample(pos, nps) + \
          random.sample(neg, min(len(neg), 64-nps))
    sub = clone(base, bag); t = tree(sub, 16); setVotes(t, lab)
    vh.append(H3(sub, t, val,  lab, grp, groups))
    th.append(H3(sub, t, test, lab, grp, groups))
  return pearson(vh, th), len(val), len(test)

if __name__ == "__main__":
  fft1.cli(the, __doc__); random.seed(the.seed)
  print("# r(val-heaven, test-heaven): ~0 = selection guessing")
  for name, file, g in SETS:
    r, nv, nt = run(name, file, g)
    print("%-9s r=%+.3f  (val=%d test=%d)" % (name, r, nv, nt))
