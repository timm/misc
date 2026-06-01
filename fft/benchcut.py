#!/usr/bin/env python3 -B
"""
benchcut.py, paired OLD-vs-NEW cut bench. same protocol as pick:
bag64/leaf16/select-on-val/test. report ms/tree + mean selected
test-heaven per set. run on each engine, eyeball the deltas.

Options:
 -s --seed seed seed=1234567891
 -m --models models models=200
 -r --reps reps reps=5
 -n --N rows N=2000
 -p --p exp p=2
 -F --Feat feat Feat=1
"""
import random, math, time
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

def H3(root, t, rows, lab, grp, groups):
  pairs = []; gp = {g: [] for g in groups}
  for r in rows:
    got = treeLeaf(root, t, r).vote
    pairs.append((lab[id(r)], got))
    if grp[id(r)] in groups: gp[grp[id(r)]].append((lab[id(r)], got))
  c   = confused(pairs).get(1, o(pd=0, prec=0))
  fpr = [confused(x).get(1, o(pf=0)).pf for x in gp.values()]
  return (c.pd, c.prec, min(fpr)/(max(fpr)+1e-32))

def heaven(m): return math.sqrt(sum((1-v)**2 for v in m))

def mktree(sub):                      # try NEW kw feat; fall to OLD
  try: return tree(sub, 16, feat=the.Feat)
  except TypeError: return tree(sub, 16)

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
  t_sum = nt = 0; picks = []
  for _ in range(the.reps):
    rows = rows0[:]; random.shuffle(rows)
    n1, n2 = int(.6*len(rows)), int(.8*len(rows))
    fit, val, test = rows[:n1], rows[n1:n2], rows[n2:]
    base = clone(d, fit)
    pos = [r for r in fit if lab[id(r)] == 1]
    neg = [r for r in fit if lab[id(r)] == 0]
    if not pos or not neg: continue
    ms = []
    for _ in range(the.models):
      x = random.randint(10, 90); nps = min(len(pos), round(64*x/100))
      bag = random.sample(pos, nps) + \
            random.sample(neg, min(len(neg), 64-nps))
      sub = clone(base, bag)
      t0 = time.process_time(); t = mktree(sub)
      t_sum += time.process_time()-t0; nt += 1
      setVotes(t, lab); ms.append((sub, t))
    best = min(ms, key=lambda m: heaven(
      H3(m[0], m[1], val, lab, grp, groups)))
    picks.append(H3(best[0], best[1], test, lab, grp, groups))
  return 1000*t_sum/nt, sum(heaven(p) for p in picks)/len(picks)

if __name__ == "__main__":
  fft1.cli(the, __doc__); random.seed(the.seed)
  print("# feat=%s  ms/tree  test-heaven (lower=better)" % the.Feat)
  tot_h = []
  for name, file, g in SETS:
    mspt, h = run(name, file, g)
    tot_h.append(h)
    print("  %-7s %6.3f ms   h=%.3f" % (name, mspt, h))
  print("  %-7s %18s=%.3f" % ("MEAN", "h", sum(tot_h)/len(tot_h)))
