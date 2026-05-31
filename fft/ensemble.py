#!/usr/bin/env python3 -B
"""
ensemble.py, does voting the whole cloud beat selecting one model?
ablation says per-model quality is config-invariant noise, so
best-on-val selection regresses to cloud-mean on test. test the
fix: predict test by MAJORITY VOTE over all M cheap models
(variance reduction) vs best-on-val single pick. higher recall +
prec + fair (closer to 1,1,1) wins. one line per dataset.

Options:
 -s --seed   seed     seed=1234567891
 -m --models models   models=300
 -r --reps   reps     reps=10
 -n --N      rows     N=1000
 -p --p      dist exp p=2
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

def score(preds, rows, lab, grp, groups):  # -> recall,prec,fair
  pairs = [(lab[id(r)], p) for r, p in zip(rows, preds)]
  gp = {g: [] for g in groups}
  for r, p in zip(rows, preds):
    if grp[id(r)] in groups: gp[grp[id(r)]].append((lab[id(r)], p))
  c   = confused(pairs).get(1, o(pd=0, prec=0))
  fpr = [confused(x).get(1, o(pf=0)).pf for x in gp.values()]
  return c.pd, c.prec, min(fpr)/(max(fpr)+1e-32)

def heaven(m): return math.sqrt(sum((1-v)**2 for v in m))

def run(name, file, gattr):
  d = Data(list(csv(file)))
  yat = next(c.at for c in d.cols.all if str(c.txt).endswith("!"))
  gat = next(c.at for c in d.cols.all if c.txt == gattr)
  lab = {id(r): (0 if r[yat] in ("?", "") else int(r[yat]))
         for r in d.rows}
  grp = {id(r): r[gat] for r in d.rows}
  groups = [g for g, _ in Counter(grp.values()).most_common(2)]
  rows0 = d.rows[:]
  if len(rows0) > the.N: rows0 = random.sample(rows0, the.N)
  ens, sel = [], []
  for _ in range(the.reps):
    rows = rows0[:]; random.shuffle(rows)
    n1, n2 = int(.6*len(rows)), int(.8*len(rows))
    fit, val, test = rows[:n1], rows[n1:n2], rows[n2:]
    base = clone(d, fit)
    pos = [r for r in fit if lab[id(r)] == 1]
    neg = [r for r in fit if lab[id(r)] == 0]
    if not pos or not neg: continue
    models = []
    for _ in range(the.models):
      x   = random.randint(10, 90)
      nps = min(len(pos), round(64*x/100))
      bag = random.sample(pos, nps) + \
            random.sample(neg, min(len(neg), 64-nps))
      sub = clone(base, bag); t = tree(sub, 16); setVotes(t, lab)
      models.append((sub, t))
    vote = lambda rs: [int(2*sum(treeLeaf(m[0], m[1], r).vote
              for m in models) > len(models)) for r in rs]
    ens.append(score(vote(test), test, lab, grp, groups))
    best = min(models, key=lambda m: heaven(score(
      [treeLeaf(m[0], m[1], r).vote for r in val],
      val, lab, grp, groups)))
    sel.append(score([treeLeaf(best[0], best[1], r).vote
                      for r in test], test, lab, grp, groups))
  avg = lambda xs: tuple(sum(c)/len(c) for c in zip(*xs))
  return avg(ens), avg(sel)

if __name__ == "__main__":
  fft1.cli(the, __doc__); random.seed(the.seed)
  print("# recall prec fair  (closer to 1,1,1 = better)")
  print("# %-7s %-22s %-22s" % ("set", "ENSEMBLE-vote",
                                 "SELECT-best-on-val"))
  for name, file, g in SETS:
    e, s = run(name, file, g)
    print("%-9s e=(%.2f %.2f %.2f) h=%.3f   s=(%.2f %.2f %.2f) h=%.3f"
          % (name, e[0], e[1], e[2], heaven(e),
             s[0], s[1], s[2], heaven(s)))
