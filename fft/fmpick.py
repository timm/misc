#!/usr/bin/env python3 -B
"""fmpick.py, pick.py protocol with fmtree (fastmap-split).
Build skeleton from K rows; route Load rows for leaf votes; pick
min-heaven on val; apply to test. Repeat reps runs.

Options:
 -s --seed   random seed       seed=1234567891
 -m --models models per run    models=30
 -r --reps   repeats           reps=20
 -K --K      build rows/tree   K=64
 -L --Load   route rows/tree   Load=1024
 -n --N      total rows        N=4000
 -p --p      dist exp          p=2
 -R --Round  decimals          Round=2
 -S --stop   leaf size         stop=8
 -g --group  protected col     group=race
 -f --file   data file
             file=/Users/timm/tmp/compas.csv
"""
import random, math
from collections import Counter
import fft1
from fft1 import (o, Data, clone, csv, confused,
                  fmtree, fmleaf, fmleaves)

the = fft1.settings(__doc__); fft1.the = the
d = lab = grp = groups = None

def load():
  global d, lab, grp, groups
  d = Data(list(csv(the.file)))
  yat = next(c.at for c in d.cols.all if str(c.txt).endswith("!"))
  gat = next(c.at for c in d.cols.all if c.txt == the.group)
  lab = {id(r): (0 if r[yat] in ("?","") else int(r[yat]))
         for r in d.rows}
  grp = {id(r): r[gat] for r in d.rows}
  groups = [g for g,_ in Counter(grp.values()).most_common(2)]

def setVotesFM(root, t, rows):
  for lf in fmleaves(t): lf.bag = []
  for r in rows: fmleaf(root, t, r).bag.append(r)
  for lf in fmleaves(t):
    s = sum(lab[id(r)] for r in lf.bag)
    lf.vote = int(2*s > len(lf.bag)) if lf.bag else 0

def metrics(root, t, rows):
  pairs, gpairs = [], {g: [] for g in groups}
  for r in rows:
    got = fmleaf(root, t, r).vote
    pairs.append((lab[id(r)], got))
    if grp[id(r)] in groups:
      gpairs[grp[id(r)]].append((lab[id(r)], got))
  c   = confused(pairs).get(1, o(pd=0, prec=0))
  fpr = [confused(gp).get(1, o(pf=0)).pf for gp in gpairs.values()]
  return c.pd, c.prec, min(fpr)/(max(fpr)+1e-32)

def heaven(m): return math.sqrt(sum((1-v)**2 for v in m))

if __name__ == "__main__":
  fft1.cli(the, __doc__)
  load()
  random.seed(the.seed)
  rows = d.rows[:]
  if len(rows) > the.N: rows = random.sample(rows, the.N)
  print("# fmpick on %s  N=%d  reps=%d  m=%d  K=%d  Load=%d" %
        (the.file.split("/")[-1], len(rows), the.reps,
         the.models, the.K, the.Load))
  print("# TAG    recall  prec    fair    heaven")
  for rep in range(the.reps):
    random.shuffle(rows)
    n1, n2 = int(.6*len(rows)), int(.8*len(rows))
    train, val, test = rows[:n1], rows[n1:n2], rows[n2:]
    best, bestd = None, 1e9
    for _ in range(the.models):
      build = random.sample(train, min(the.K, len(train)))
      sub   = clone(d, build)
      t     = fmtree(sub, stop=the.stop)
      pool  = random.sample(train, min(the.Load, len(train)))
      setVotesFM(sub, t, pool)
      mv = metrics(sub, t, val)
      h  = heaven(mv)
      if h < bestd: bestd, best = h, (sub, t)
    if best is None: continue
    mt = metrics(best[0], best[1], test)
    print("TEST   %.4f  %.4f  %.4f  %.4f" % (mt[0], mt[1], mt[2], heaven(mt)))
