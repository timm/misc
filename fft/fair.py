#!/usr/bin/env python3 -B
"""
fair.py, fft1 model-cloud vs Cruz-2021 Fig.1 (COMPAS panel).
Build N fastmap trees; each (tree, threshold) = one (recall,
fairness) point.  recall=TPR.  fairness=predictive equality
= min/max FPR across race groups (1=fair).  Print spread +
Pareto front.  (AOF panel is Feedzai in-house, unavailable;
COMPAS is the public panel from the same figure.)

Options:
 -s --seed  random seed   seed=1234567891
 -t --trees number trees  trees=100
 -n --N     row sample     N=1000
 -f --file  data file     file=/Users/timm/gits/moot/classify/COMPAS53.csv
"""
import random, re
import fft1
from fft1 import o, csv, Data, fastmap, leaf, coerce

the = o(**{k: coerce(v) for k, v in re.findall(r"(\w+)=(\S+)", __doc__)})

# pre-decision features only (no decile_score / is_recid / r_*).
# Upper=numeric, lower=symbolic (fft1 col convention).
FEATS = [("Age","Age"), ("Priors_count","Priors"),
         ("Juv_fel_count","JuvFel"), ("Juv_misd_count","JuvMisd"),
         ("Juv_other_count","JuvOther"), ("sex","sex"),
         ("race","race"), ("c_charge_degree","charge")]
LABEL, GROUP = "two_year_recid!", "race"

def load():
  raw  = list(csv(the.file)); head = raw[0]; body = raw[1:]
  ix   = {n: i for i, n in enumerate(head)}
  random.seed(the.seed)
  if len(body) > the.N: body = random.sample(body, the.N)
  names = [nm for _, nm in FEATS]
  rows, lab, grp = [], {}, {}
  for r in body:
    fr = [r[ix[c]] for c, _ in FEATS]      # feature row (tree input)
    rows.append(fr)
    lab[id(fr)] = 0 if r[ix[LABEL]] in ("?", "") else int(r[ix[LABEL]])
    grp[id(fr)] = r[ix[GROUP]]
  random.shuffle(rows)
  cut = len(rows) // 2                      # 50/50 train/test
  train, test = rows[:cut], rows[cut:]
  return Data([names] + train), test, lab, grp

def score(train, tree, test, lab):         # test row -> train-leaf mean
  out = {}
  for r in test:
    L = leaf(train, tree, r)               # route test row into train tree
    out[id(r)] = sum(lab[id(x)] for x in L.rows) / len(L.rows)
  return out

def point(test, sc, alert, lab, grp, groups):
  vals = sorted((sc[id(r)] for r in test), reverse=True)
  thr  = vals[min(int(alert * len(vals)), len(vals) - 1)]  # top alert%
  tp = fn = 0
  fp = {g: 0 for g in groups}; tn = {g: 0 for g in groups}
  for r in test:
    pos = sc[id(r)] >= thr; y = lab[id(r)]; g = grp[id(r)]
    if y == 1: tp += pos; fn += not pos
    elif g in groups:
      fp[g] += pos; tn[g] += not pos
  recall = tp / (tp + fn + 1e-32)
  fpr = [fp[g] / (fp[g] + tn[g] + 1e-32) for g in groups]
  fair = min(fpr) / (max(fpr) + 1e-32)
  return recall, fair

def pareto(pts):                           # maximise both
  out = []
  for p in pts:
    if not any(q[0] >= p[0] and q[1] >= p[1] and q != p for q in pts):
      out.append(p)
  return sorted(out)

if __name__ == "__main__":
  fft1.cli(the); fft1.the.trees = the.trees
  train, test, lab, grp = load()
  from collections import Counter
  groups = [g for g, _ in Counter(grp.values()).most_common(2)]
  print("train", len(train.rows), "test", len(test), "groups", groups)
  pts = []
  for _ in range(the.trees):
    tree = fastmap(train)                  # model = one tree on train
    sc   = score(train, tree, test, lab)   # score held-out test
    for alert in (.1, .2, .3, .4, .5):     # fixed operating points
      pts.append(point(test, sc, alert, lab, grp, groups))
  rec = sorted(p[0] for p in pts); far = sorted(p[1] for p in pts)
  q = lambda xs, p: xs[int(p * (len(xs) - 1))]
  print("points        ", len(pts))
  print("recall   min/med/max %.2f %.2f %.2f" % (rec[0], q(rec,.5), rec[-1]))
  print("fairness min/med/max %.2f %.2f %.2f" % (far[0], q(far,.5), far[-1]))
  pf = pareto(pts)
  print("pareto front (%d pts):" % len(pf))
  for r, f in pf: print("  recall %.2f  fairness %.2f" % (r, f))

  import matplotlib; matplotlib.use("Agg")
  import matplotlib.pyplot as plt
  plt.figure(figsize=(5, 5))
  plt.scatter([p[0] for p in pts], [p[1] for p in pts],
              s=10, alpha=.3, color="steelblue", label="%d models" % len(pts))
  plt.plot([p[0] for p in pf], [p[1] for p in pf],
           "-o", color="crimson", ms=4, label="Pareto front")
  plt.xlabel("Predictive performance (recall)")
  plt.ylabel("Fairness (predictive equality)")
  plt.title("%d fastmap trees on COMPAS (held-out)" % the.trees)
  plt.xlim(0, 1); plt.ylim(0, 1); plt.grid(alpha=.3); plt.legend()
  out = "/Users/timm/tmp/fair.png"
  import os; os.makedirs("/Users/timm/tmp", exist_ok=True)
  plt.tight_layout(); plt.savefig(out, dpi=120)
  print("wrote", out)
