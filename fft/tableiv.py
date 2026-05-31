#!/usr/bin/env python3 -B
"""tableiv.py, Cruz-2021 Table-style result for OUR method on COMPAS.
60/20/20 train/val/test. Generate a random cloud (class-ratio bags),
select by g = 0.5*recall + 0.5*fairness on VALIDATION, report val+test."""
import random
from collections import Counter
import fft1, compas
from fft1 import Data, clone, rmap

N = 512
random.seed(fft1.the.seed)
names, rows, lab, grp = compas.features()
random.shuffle(rows)
a, b   = int(.6*len(rows)), int(.8*len(rows))
train, val, test = rows[:a], rows[a:b], rows[b:]
base   = Data([names] + train)
pos    = [r for r in train if lab[id(r)] == 1]
neg    = [r for r in train if lab[id(r)] == 0]
groups = [g for g, _ in Counter(grp.values()).most_common(2)]

TAU = 0.10                             # COMPAS target: FPR (pf) <= 10%

def score(tree, data):                 # -> (precision, fairness, pf)
  r = compas.evaluate(train_d, tree, data, lab, grp, groups)
  return r[3], r[1], r[2]              # prec, fair, pf

models = []
for _ in range(N):
  x   = random.randint(10, 90)
  np_ = min(len(pos), round(128*x/100))
  bag = random.sample(pos, np_) + random.sample(neg, min(len(neg), 128-np_))
  train_d = clone(base, bag)
  tree    = rmap(train_d)
  vp, vf, vpf = score(tree, val)
  tp, tf, tpf = score(tree, test)
  models.append((vp, vf, vpf, tp, tf))

feasible = [m for m in models if m[2] <= TAU]          # pf_val <= TAU
g        = lambda m: 0.5*m[0] + 0.5*m[1]               # scalarize prec + fair
best     = max(feasible, key=g)                        # select on validation
hi_p     = max(feasible, key=lambda m: m[0])           # best feasible precision
hi_f     = max(feasible, key=lambda m: m[1])           # best feasible fairness

print("COMPAS, %d random models, 60/20/20, FPR<=%.0f%% (matches Table III)\n" % (N, TAU*100))
print("%-22s %15s %15s" % ("", "Validation", "Test"))
print("%-22s %7s %7s %7s %7s" % ("select-by", "Prec", "Fair", "Prec", "Fair"))
for tag, m in [("g=.5P+.5F", best), ("max precision", hi_p), ("max fairness", hi_f)]:
  print("%-22s %7.1f %7.1f %7.1f %7.1f" %
        (tag, m[0]*100, m[1]*100, m[3]*100, m[4]*100))
print("\nfeasible models (pf<=%.2f): %d / %d" % (TAU, len(feasible), N))
