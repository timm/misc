#!/usr/bin/env python3 -B
"""
rf.py, random-forest variant vs current L2. does a random column
subspace (feat) + bigger random bags rescue the all-cols best-cut
that L3 found collapses the cloud? measures cloud SPREAD (sd of
recall/prec/fair = diversity) + select-on-val test-heaven + ms.

configs (bag, feat):
 L2     ratio-64  feat=1     current (one random col)
 rf-sq  rand-256  feat=sqrt  per-node sqrt(n) subspace (RF)
 rf-all rand-256  feat=all   best cut over ALL cols (was L3)
 sq-64  ratio-64  feat=sqrt  isolate: subspace at small bag

Options:
 -s --seed seed seed=1234567891
 -m --models models models=200
 -r --reps reps reps=5
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
#       name      bag      feat
CFG = [("L2",     "r64",   1),
       ("rf-sq",  "n256",  "sqrt"),
       ("rf-all", "n256",  "all"),
       ("sq-64",  "r64",   "sqrt")]

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

def sd(xs):
  n = len(xs)
  if n < 2: return 0
  mu = sum(xs)/n
  return (sum((x-mu)**2 for x in xs)/(n-1))**.5

def mkbag(pos, neg, how):
  if how == "n256":                   # plain random 256 (your idea)
    both = pos + neg
    return random.sample(both, min(256, len(both)))
  x = random.randint(10, 90)          # ratio sweep, bag 64
  nps = min(len(pos), round(64*x/100))
  return random.sample(pos, nps) + random.sample(neg, min(len(neg), 64-nps))

def run(file, gattr):
  d = Data(list(csv(file)))
  yat = next(c.at for c in d.cols.all if str(c.txt).endswith("!"))
  gat = next(c.at for c in d.cols.all if c.txt == gattr)
  lab = {id(r): (0 if r[yat] in ("?", "") else int(r[yat]))
         for r in d.rows}
  grp = {id(r): r[gat] for r in d.rows}
  groups = [g for g, _ in Counter(grp.values()).most_common(2)]
  rows0 = d.rows[:]
  if len(rows0) > the.N: rows0 = random.sample(rows0, the.N)
  out = {c[0]: dict(t=0.0, nt=0, picks=[], spread=[]) for c in CFG}
  for _ in range(the.reps):
    rows = rows0[:]; random.shuffle(rows)
    n1, n2 = int(.6*len(rows)), int(.8*len(rows))
    fit, val, test = rows[:n1], rows[n1:n2], rows[n2:]
    base = clone(d, fit)
    pos = [r for r in fit if lab[id(r)] == 1]
    neg = [r for r in fit if lab[id(r)] == 0]
    if not pos or not neg: continue
    for nm, how, feat in CFG:
      ms = []; cloud = []
      for _ in range(the.models):
        bag = mkbag(pos, neg, how)
        sub = clone(base, bag)
        t0 = time.process_time()
        t = tree(sub, 16, feat=feat)
        out[nm]["t"] += time.process_time()-t0; out[nm]["nt"] += 1
        setVotes(t, lab)
        ms.append((sub, t))
        cloud.append(H3(sub, t, val, lab, grp, groups))
      best = min(ms, key=lambda m: heaven(
        H3(m[0], m[1], val, lab, grp, groups)))
      out[nm]["picks"].append(H3(best[0], best[1], test, lab, grp, groups))
      # cloud spread = mean per-axis sd (bigger = more diverse)
      sp = sum(sd([p[i] for p in cloud]) for i in range(3))/3
      out[nm]["spread"].append(sp)
  return out

if __name__ == "__main__":
  fft1.cli(the, __doc__); random.seed(the.seed)
  print("# cfg     ms/tree  spread  test-heaven (h lo=better,"
        " spread hi=diverse)")
  agg = {c[0]: dict(t=0.0, nt=0, picks=[], spread=[]) for c in CFG}
  for name, file, g in SETS:
    res = run(file, g)
    print("\n# %s" % name)
    for nm, _, _ in CFG:
      r = res[nm]; mspt = 1000*r["t"]/r["nt"]
      h = sum(heaven(p) for p in r["picks"])/len(r["picks"])
      sp = sum(r["spread"])/len(r["spread"])
      print("  %-7s %6.3f ms  %.3f   h=%.3f" % (nm, mspt, sp, h))
      for k in ("t", "nt"): agg[nm][k] += r[k]
      agg[nm]["picks"] += r["picks"]; agg[nm]["spread"] += r["spread"]
  print("\n# ALL (mean over 4 sets)")
  for nm, _, _ in CFG:
    r = agg[nm]; mspt = 1000*r["t"]/r["nt"]
    h = sum(heaven(p) for p in r["picks"])/len(r["picks"])
    sp = sum(r["spread"])/len(r["spread"])
    print("  %-7s %6.3f ms  %.3f   h=%.3f" % (nm, mspt, sp, h))
