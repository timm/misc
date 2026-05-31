#!/usr/bin/env python3 -B
"""
fairml.py, recall/fairness model cloud on ANY moot-format CSV.
(c) 2025, Tim Menzies <timm@ieee.org>, MIT license

Generic version of compas.py: features come from the header tags
(fft1 parses them), label = the column ending '!', protected
attribute = -g.  stdout: "recall fair pf prec acc" per model.

Options:
 -s --seed    random seed   seed=1234567891
 -t --trees   models        trees=512
 -n --N       row subsample N=4000
 -p --p       distance exp  p=2
 -R --Round   repr decimals Round=2
 -S --stop    leaf size     stop=None
 -g --group   protected col group=race
 -f --file    data file     file=/Users/timm/tmp/adult.csv
"""
import random, os
from collections import Counter
import fft1
from fft1 import o, Data, clone, rmap, leaf, confused, csv

the = fft1.settings(__doc__)
fft1.the = the

def load():
  d   = Data(list(csv(the.file)))                  # fft1 parses header tags
  yat = next(c.at for c in d.cols.all if str(c.txt).endswith("!"))
  gat = next(c.at for c in d.cols.all if c.txt == the.group)
  rows = d.rows
  if len(rows) > the.N: rows = random.sample(rows, the.N)
  lab = {id(r): (0 if r[yat] in ("?", "") else int(r[yat])) for r in rows}
  grp = {id(r): r[gat] for r in rows}
  return d, rows, lab, grp

def evaluate(train, tree, test, lab, grp, groups):
  pairs = []; gpairs = {g: [] for g in groups}
  for r in test:
    L    = leaf(train, tree, r)
    got  = int(2*sum(lab[id(x)] for x in L.rows) > len(L.rows))   # leaf majority
    want = lab[id(r)]; g = grp[id(r)]
    pairs.append((want, got))
    if g in groups: gpairs[g].append((want, got))
  c    = confused(pairs).get(1, o(pd=0, pf=0, prec=0, acc=0))
  fpr  = [confused(gp).get(1, o(pf=0)).pf for gp in gpairs.values()]
  fair = min(fpr)/(max(fpr)+1e-32)
  return c.pd, fair, c.pf, c.prec, c.acc

if __name__ == "__main__":
  fft1.cli(the, __doc__)
  print("# %s / %s" % (os.path.basename(the.file).split(".")[0], the.group))
  random.seed(the.seed)
  d, rows, lab, grp = load()
  groups = [g for g, _ in Counter(grp[id(r)] for r in rows).most_common(2)]
  random.shuffle(rows)
  cut        = int(.8*len(rows))
  pool, test = rows[:cut], rows[cut:]
  base       = clone(d, pool)
  pos = [r for r in pool if lab[id(r)] == 1]
  neg = [r for r in pool if lab[id(r)] == 0]
  for _ in range(the.trees):
    x   = random.randint(10, 90)
    nps = min(len(pos), round(128*x/100))
    bag = random.sample(pos, nps) + random.sample(neg, min(len(neg), 128-nps))
    sub = clone(base, bag)
    print("%.4f %.4f %.4f %.4f %.4f" %
          evaluate(sub, rmap(sub), test, lab, grp, groups))
