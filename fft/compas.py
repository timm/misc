#!/usr/bin/env python3 -B
"""
compas.py, fairness (acc vs predictive-equality) cloud on COMPAS.
(c) 2025, Tim Menzies <timm@ieee.org>, MIT license

Uses the fft1 engine; only this file knows COMPAS's protected
attribute + label.  stdout: one line per model = "acc fairness".
repeats x trees models; each shuffle splits 80/20, learns trees
on train, scores each tree on the 20% holdout.

Options:
 -s --seed    random seed   seed=1234567891
 -t --trees   trees/shuffle trees=200
 -r --repeats shuffles      repeats=1
 -n --N       row subsample N=99999
 -p --p       distance exponent p=2
 -R --Round   repr decimals Round=2
 -S --stop    leaf size     stop=None
 -g --group   protected col group=race
 -f --file    data file     file=/Users/timm/gits/moot/classify/COMPAS53.csv
"""
import random
from collections import Counter
import fft1
from fft1 import o, Data, clone, rmap, leaf, confused, csv

the = fft1.settings(__doc__)
fft1.the = the                 # engine reads fft1.the.stop/p/Round/...

# pre-decision features (Upper=numeric, lower=symbolic) ----------
FEATS = [("Age","Age"), ("Priors_count","Priors"),
         ("Juv_fel_count","JuvFel"), ("Juv_misd_count","JuvMisd"),
         ("Juv_other_count","JuvOther"), ("sex","sex"),
         ("race","race"), ("c_charge_degree","charge")]
LABEL = "two_year_recid!"

def features():               # -> names, rows(feature lists), lab, grp
  raw = list(csv(the.file)); head = raw[0]
  ix  = {n: j for j, n in enumerate(head)}
  names, rows, lab, grp = [nm for _, nm in FEATS], [], {}, {}
  for r in raw[1:]:
    fr = [r[ix[c]] for c, _ in FEATS]
    rows.append(fr)
    grp[id(fr)] = r[ix[the.group]]                    # protected attribute
    lab[id(fr)] = 0 if r[ix[LABEL]] in ("?","") else int(r[ix[LABEL]])
  return names, rows, lab, grp

def evaluate(train, tree, test, lab, grp, groups):
  pairs = []; gpairs = {g: [] for g in groups}   # (want,got) all + per group
  for r in test:
    L    = leaf(train, tree, r)                  # leaf of train rows
    got  = int(2 * sum(lab[id(x)] for x in L.rows) > len(L.rows))  # leaf majority
    want = lab[id(r)]; g = grp[id(r)]
    pairs.append((want, got))
    if g in groups: gpairs[g].append((want, got))
  c    = confused(pairs).get(1, o(pd=0, pf=0, prec=0, acc=0))  # positive class
  fpr  = [confused(gp).get(1, o(pf=0)).pf for gp in gpairs.values()]
  fair = min(fpr)/(max(fpr)+1e-32)                           # predictive equality
  return c.pd, fair, c.pf, c.prec, c.acc                     # recall,fair,pf,prec,acc

if __name__ == "__main__":
  import os
  fft1.cli(the, __doc__)
  print("# %s / %s" % (os.path.basename(the.file).split(".")[0], the.group))  # title
  random.seed(the.seed)
  names, rows, lab, grp = features()
  if len(rows) > the.N: rows = random.sample(rows, the.N)
  groups = [g for g, _ in Counter(grp.values()).most_common(2)]
  for _ in range(the.repeats):
    random.shuffle(rows)
    cut        = int(.8 * len(rows))
    pool, test = rows[:cut], rows[cut:]           # 80% pool, 20% holdout
    base       = Data([names] + pool)             # template for col names
    pos = [r for r in pool if lab[id(r)] == 1]    # split pool by class
    neg = [r for r in pool if lab[id(r)] == 0]
    for _ in range(the.trees):
      x    = random.randint(10, 90)               # % positives in the bag
      npos = min(len(pos), round(128 * x / 100))
      nneg = min(len(neg), 128 - npos)
      bag  = random.sample(pos, npos) + random.sample(neg, nneg)
      sub  = clone(base, bag)                     # class-ratio bag (the one knob)
      recall, fair, pf, prec, acc = evaluate(sub, rmap(sub), test, lab, grp, groups)
      print("%.4f %.4f %.4f %.4f %.4f" % (recall, fair, pf, prec, acc))
