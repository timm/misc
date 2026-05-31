#!/usr/bin/env python3 -B
"""
pick.py, honest train/select/test loop on a moot CSV.
reps times: shuffle; 80 train / 20 test; build a model cloud on a
fit-slice of train, pick the model nearest heaven (recall,prec,fair
all =1) on a validation-slice of train; report that model on test.

Options:
 -s --seed   random seed   seed=1234567891
 -t --trees  cloud size    trees=300
 -r --reps   repeats       reps=25
 -n --N      row subsample N=4000
 -p --p      distance exp  p=2
 -R --Round  repr decimals Round=2
 -S --stop   leaf size     stop=None
 -P --proj   2-pole project proj=False
 -g --group  protected col group=race
 -f --file   data file     file=/Users/timm/gits/moot/classify/COMPAS53.csv
"""
import random, os, math
from collections import Counter
import fft1
from fft1 import o, Data, clone, rmap, leaf, rmapf, leaff, confused, csv

the = fft1.settings(__doc__); fft1.the = the
RMAP, LEAF = rmapf, leaff                          # fast 1-pole default; -P=proj

d = lab = grp = groups = None                    # set in load() after cli

def load():
  global d, lab, grp, groups
  d   = Data(list(csv(the.file)))
  yat = next(c.at for c in d.cols.all if str(c.txt).endswith("!"))
  gat = next(c.at for c in d.cols.all if c.txt == the.group)
  lab = {id(r): (0 if r[yat] in ("?", "") else int(r[yat])) for r in d.rows}
  grp = {id(r): r[gat] for r in d.rows}
  groups = [g for g, _ in Counter(grp.values()).most_common(2)]

def metrics(train, tree, rows):                  # -> (recall, prec, fair)
  pairs = []; gpairs = {g: [] for g in groups}
  for r in rows:
    L   = LEAF(train, tree, r)
    got = int(2*sum(lab[id(x)] for x in L.rows) > len(L.rows))
    pairs.append((lab[id(r)], got))
    if grp[id(r)] in groups: gpairs[grp[id(r)]].append((lab[id(r)], got))
  c   = confused(pairs).get(1, o(pd=0, prec=0))
  fpr = [confused(gp).get(1, o(pf=0)).pf for gp in gpairs.values()]
  return c.pd, c.prec, min(fpr)/(max(fpr)+1e-32)

def heaven(m): return math.sqrt(sum((1-v)**2 for v in m))   # dist to (1,1,1)

if __name__ == "__main__":
  fft1.cli(the, __doc__)
  if the.proj: RMAP, LEAF = rmap, leaf            # opt into 2-pole projection
  globals().update(RMAP=RMAP, LEAF=LEAF)
  load()                                          # load AFTER cli sets -f/-g
  random.seed(the.seed)
  rows = d.rows[:]
  if len(rows) > the.N: rows = random.sample(rows, the.N)
  name = "%s / %s" % (os.path.basename(the.file).split(".")[0], the.group)
  cloud, te_rows = [], []
  for _ in range(the.reps):
    random.shuffle(rows)
    n1, n2 = int(.6*len(rows)), int(.8*len(rows))
    fit, val, test = rows[:n1], rows[n1:n2], rows[n2:]      # 60/20/20
    base = clone(d, fit)
    pos  = [r for r in fit if lab[id(r)] == 1]
    neg  = [r for r in fit if lab[id(r)] == 0]
    best, bestd = None, 1e9
    for _ in range(the.trees):
      x   = random.randint(10, 90)
      nps = min(len(pos), round(128*x/100))
      bag = random.sample(pos, nps) + random.sample(neg, min(len(neg), 128-nps))
      sub = clone(base, bag); tree = RMAP(sub)
      mv  = metrics(sub, tree, val)                         # (recall,prec,fair)
      cloud.append(mv)                                      # the whole space
      if heaven(mv) < bestd: bestd, best = heaven(mv), (sub, tree)
    te_rows.append(metrics(best[0], best[1], test))         # selected -> test
  print("#", name)                                          # rows: TAG recall fair prec
  for r, p, f in cloud:   print("CLOUD %.4f %.4f %.4f" % (r, f, p))
  for r, p, f in te_rows: print("TEST  %.4f %.4f %.4f" % (r, f, p))
