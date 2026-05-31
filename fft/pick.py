#!/usr/bin/env python3 -B
"""
pick.py, honest train/select/test loop on a moot CSV.
reps times: shuffle; 80 train / 20 test; build a model cloud on a
fit-slice of train, pick the model nearest heaven (recall,prec,fair
all =1) on a validation-slice of train; report that model on test.

Options:
 -s --seed   random seed     seed=1234567891
 -m --models models per run  models=300
 -r --reps   repeats         reps=25
 -n --N      total rows (60/20/20) N=4000
 -v --Val    max val rows    Val=99999
 -p --p      distance exp    p=2
 -R --Round  repr decimals Round=2
 -S --stop   leaf size     stop=None
 -g --group  protected col group=race
 -f --file   data file     file=/Users/timm/gits/moot/classify/COMPAS53.csv
"""
import random, os, math
from collections import Counter
import fft1
from fft1 import o, Data, clone, tree, treeLeaf, confused, csv

the = fft1.settings(__doc__); fft1.the = the

d = lab = grp = groups = None                    # set in load() after cli

def load():
  global d, lab, grp, groups
  d   = Data(list(csv(the.file)))
  yat = next(c.at for c in d.cols.all if str(c.txt).endswith("!"))
  gat = next(c.at for c in d.cols.all if c.txt == the.group)
  lab = {id(r): (0 if r[yat] in ("?", "") else int(r[yat])) for r in d.rows}
  grp = {id(r): r[gat] for r in d.rows}
  groups = [g for g, _ in Counter(grp.values()).most_common(2)]

def setVotes(t):                              # leaf majority, computed ONCE
  for lf in leaves(t):
    lf.vote = int(2*sum(lab[id(r)] for r in lf.rows) > len(lf.rows))

def metrics(train, t, rows):                  # -> (recall, prec, fair)
  pairs = []; gpairs = {g: [] for g in groups}
  for r in rows:
    got = treeLeaf(train, t, r).vote          # precomputed -> just a lookup
    pairs.append((lab[id(r)], got))
    if grp[id(r)] in groups: gpairs[grp[id(r)]].append((lab[id(r)], got))
  c   = confused(pairs).get(1, o(pd=0, prec=0))
  fpr = [confused(gp).get(1, o(pf=0)).pf for gp in gpairs.values()]
  return c.pd, c.prec, min(fpr)/(max(fpr)+1e-32)

def leaves(t):                                # walk leaf Datas of a tree
  if t.it is fft1.Data: yield t
  else: yield from leaves(t.left); yield from leaves(t.right)

def trainScore(t):                            # confusion from leaf rows; no eval
  pairs = []; gp = {g: [] for g in groups}    # each leaf votes its own rows
  for lf in leaves(t):
    got = int(2*sum(lab[id(r)] for r in lf.rows) > len(lf.rows))
    for r in lf.rows:
      pairs.append((lab[id(r)], got))
      if grp[id(r)] in groups: gp[grp[id(r)]].append((lab[id(r)], got))
  c   = confused(pairs).get(1, o(pd=0, prec=0))
  fpr = [confused(g).get(1, o(pf=0)).pf for g in gp.values()]
  return c.pd, c.prec, min(fpr)/(max(fpr)+1e-32)

def heaven(m): return math.sqrt(sum((1-v)**2 for v in m))   # dist to (1,1,1)

if __name__ == "__main__":
  fft1.cli(the, __doc__)
  load()                                          # load AFTER cli sets -f/-g
  random.seed(the.seed)
  rows = d.rows[:]
  if len(rows) > the.N: rows = random.sample(rows, the.N)
  name = "%s / %s" % (os.path.basename(the.file).split(".")[0], the.group)
  cloud, te_rows = [], []
  for i in range(the.reps):
    random.shuffle(rows)
    n1, n2 = int(.6*len(rows)), int(.8*len(rows))
    fit, val, test = rows[:n1], rows[n1:n2], rows[n2:]      # 60/20/20
    if len(val) > the.Val: val = random.sample(val, the.Val)  # cap if huge
    base = clone(d, fit)
    pos  = [r for r in fit if lab[id(r)] == 1]
    neg  = [r for r in fit if lab[id(r)] == 0]
    best, bestd = None, 1e9
    for _ in range(the.models):
      x   = random.randint(10, 90)
      nps = min(len(pos), round(64*x/100))    # bag 64: ablation-cheap
      bag = random.sample(pos, nps) + random.sample(neg, min(len(neg), 64-nps))
      sub = clone(base, bag); t = tree(sub, 16); setVotes(t)  # leaf 16
      mv  = metrics(sub, t, val)                         # held-out selection
      if i == 0: cloud.append(mv)                        # train chart = 1 run
      if heaven(mv) < bestd: bestd, best = heaven(mv), (sub, t)
    te_rows.append(metrics(best[0], best[1], test))         # selected -> test
  print("#", name)                                          # rows: TAG recall fair prec
  for r, p, f in cloud:   print("CLOUD %.4f %.4f %.4f" % (r, f, p))
  for r, p, f in te_rows: print("TEST  %.4f %.4f %.4f" % (r, f, p))
