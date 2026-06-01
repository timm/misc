#!/usr/bin/env python3 -B
"""
bins.py, does decile cut (vs all values) speed up + keep quality?
PAIRED: per rep build M bags ONCE, then build each bag's tree at
bins in {0=all,5,10,20}. time build, select best-on-val, test.
report ms/tree + mean selected test-heaven per dataset. same bags
across bins => clean isolation of the candidate-set knob.

Options:
 -s --seed seed seed=1234567891
 -m --models models models=300
 -r --reps reps reps=8
 -n --N rows N=2000
 -p --p exp p=2
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
BINS = [0, 5, 10, 20]

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
  res = {b: dict(t=0.0, nt=0, picks=[]) for b in BINS}
  for _ in range(the.reps):
    rows = rows0[:]; random.shuffle(rows)
    n1, n2 = int(.6*len(rows)), int(.8*len(rows))
    fit, val, test = rows[:n1], rows[n1:n2], rows[n2:]
    base = clone(d, fit)
    pos = [r for r in fit if lab[id(r)] == 1]
    neg = [r for r in fit if lab[id(r)] == 0]
    if not pos or not neg: continue
    bags = []                              # SAME bags for all bins
    for _ in range(the.models):
      x = random.randint(10, 90); nps = min(len(pos), round(64*x/100))
      bags.append(random.sample(pos, nps) +
                  random.sample(neg, min(len(neg), 64-nps)))
    for b in BINS:
      ms = []
      for bag in bags:
        sub = clone(base, bag)
        t0 = time.process_time()
        t = tree(sub, 16, bins=b)
        res[b]["t"] += time.process_time()-t0; res[b]["nt"] += 1
        setVotes(t, lab); ms.append(t)
      best = min(ms, key=lambda t: heaven(H3(base, t, val, lab, grp, groups)))
      res[b]["picks"].append(H3(base, best, test, lab, grp, groups))
  return res

if __name__ == "__main__":
  fft1.cli(the, __doc__); random.seed(the.seed)
  print("# bins  ms/tree  speedup  test-heaven (lower=better)")
  agg = {b: dict(t=0.0, nt=0, picks=[]) for b in BINS}
  for name, file, g in SETS:
    res = run(name, file, g)
    print("\n# %s" % name)
    for b in BINS:
      r = res[b]; mspt = 1000*r["t"]/r["nt"]
      h = sum(heaven(p) for p in r["picks"])/len(r["picks"])
      print("  bins=%-2d %6.3f ms   %4.1fx   h=%.3f"
            % (b, mspt, (1000*res[0]["t"]/res[0]["nt"])/mspt, h))
      agg[b]["t"] += r["t"]; agg[b]["nt"] += r["nt"]
      agg[b]["picks"] += r["picks"]
  print("\n# ALL (mean over 4 sets)")
  base = 1000*agg[0]["t"]/agg[0]["nt"]
  for b in BINS:
    r = agg[b]; mspt = 1000*r["t"]/r["nt"]
    h = sum(heaven(p) for p in r["picks"])/len(r["picks"])
    print("  bins=%-2d %6.3f ms   %4.1fx   h=%.3f"
          % (b, mspt, base/mspt, h))
