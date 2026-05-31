#!/usr/bin/env python3 -B
"""
sup.py, does SUPERVISED cut lift precision above base rate?
diversification operators are inert (see ablate.py) + selection
works (valtest.py r~.95). so the dot ceiling is the SPLIT being
label-blind: random-cut leaves ~ base-rate pure -> precision
pinned at base rate (pale dots). test the fix: pick the cut with
best gini-gain among K random candidates (ExtraTrees-style). same
bag/leaf/select protocol. report selected test (recall,prec,fair)
RANDOM vs SUP, and base rate. precision >> base = real lift.

Options:
 -s --seed seed seed=1234567891
 -m --models models models=300
 -r --reps reps reps=10
 -n --N rows N=1000
 -k --K cand cuts K=8
 -p --p exp p=2
"""
import random, math
from collections import Counter
import fft1
from fft1 import o, Data, clone, treeLeaf, confused, csv

the = fft1.settings(__doc__); fft1.the = the
H = "/Users/timm/tmp/"
SETS = [("compas", H+"compas.csv", "race"),
        ("adult",  H+"adult.csv",  "race"),
        ("dutch",  H+"dutchf.csv", "sex"),
        ("diab",   H+"diab.csv",   "race")]

def Tree(): pass

def gini(rows, lab):
  if not rows: return 0
  p = sum(lab[id(r)] for r in rows)/len(rows)
  return 1 - p*p - (1-p)*(1-p)

def splitBy(rows, c, v):
  sym = c.it is not fft1.Num
  ok, no = [], []
  for r in rows:
    x = r[c.at]
    go = x != "?" and (x == v if sym else x <= v)
    (ok if go else no).append(r)
  return ok, no, sym

def build(d, cols, stop, lab, sup):
  def grow(rows):
    if len(rows) <= stop or gini(rows, lab) == 0:
      return clone(d, rows)
    best = None
    tries = the.K if sup else 1
    for _ in range(10 if not sup else 1):
      cand = []
      for _ in range(tries):              # K candidates if sup
        c = random.choice(cols)
        v = random.choice(rows)[c.at]
        if v == "?": continue
        ok, no, sym = splitBy(rows, c, v)
        if not ok or not no: continue
        n = len(rows)
        gain = gini(rows, lab) - (
          len(ok)*gini(ok, lab) + len(no)*gini(no, lab))/n
        cand.append((gain, c, v, sym, ok, no))
      if cand:
        best = max(cand, key=lambda z: z[0]) if sup else cand[0]
        break
    if not best: return clone(d, rows)
    _, c, v, sym, ok, no = best
    return o(it=Tree, at=c.at, sym=sym, cut=v,
             left=grow(ok), right=grow(no))
  return grow(d.rows)

def route(t, row):
  while t.it is Tree:
    x = row[t.at]
    go = x != "?" and (x == t.cut if t.sym else x <= t.cut)
    t = t.left if go else t.right
  return t

def leaves(t):
  if t.it is fft1.Data: yield t
  else: yield from leaves(t.left); yield from leaves(t.right)

def setVotes(t, lab):
  for lf in leaves(t):
    s = sum(lab[id(r)] for r in lf.rows)
    lf.vote = int(2*s > len(lf.rows))

def H3(t, rows, lab, grp, groups):
  pairs = []; gp = {g: [] for g in groups}
  for r in rows:
    got = route(t, r).vote
    pairs.append((lab[id(r)], got))
    if grp[id(r)] in groups: gp[grp[id(r)]].append((lab[id(r)], got))
  c   = confused(pairs).get(1, o(pd=0, prec=0))
  fpr = [confused(x).get(1, o(pf=0)).pf for x in gp.values()]
  return (c.pd, c.prec, min(fpr)/(max(fpr)+1e-32))

def heaven(m): return math.sqrt(sum((1-v)**2 for v in m))

def cloud(base, cols, pos, neg, lab, sup):
  ms = []
  for _ in range(the.models):
    x   = random.randint(10, 90)
    nps = min(len(pos), round(64*x/100))
    bag = random.sample(pos, nps) + \
          random.sample(neg, min(len(neg), 64-nps))
    sub = clone(base, bag)
    t   = build(sub, cols, 16, lab, sup); setVotes(t, lab)
    ms.append((sub, t))
  return ms

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
  base_rate = sum(lab[id(r)] for r in rows0)/len(rows0)
  out = {0: [], 1: []}
  for _ in range(the.reps):
    rows = rows0[:]; random.shuffle(rows)
    n1, n2 = int(.6*len(rows)), int(.8*len(rows))
    fit, val, test = rows[:n1], rows[n1:n2], rows[n2:]
    base = clone(d, fit); cols = d.cols.x
    pos = [r for r in fit if lab[id(r)] == 1]
    neg = [r for r in fit if lab[id(r)] == 0]
    if not pos or not neg: continue
    for sup in (0, 1):
      ms = cloud(base, cols, pos, neg, lab, sup)
      best = min(ms, key=lambda m: heaven(
        H3(m[1], val, lab, grp, groups)))
      out[sup].append(H3(best[1], test, lab, grp, groups))
  avg = lambda xs: tuple(sum(c)/len(c) for c in zip(*xs))
  return base_rate, avg(out[0]), avg(out[1])

if __name__ == "__main__":
  fft1.cli(the, __doc__); random.seed(the.seed)
  print("# selected test (recall prec fair); base=base rate")
  print("# %-7s %-6s %-26s %-26s" % ("set", "base",
        "RANDOM-cut", "SUP-cut (K=%d)" % the.K))
  for name, file, g in SETS:
    br, r0, r1 = run(name, file, g)
    print("%-9s %.2f  rnd=(%.2f %.2f %.2f)  sup=(%.2f %.2f %.2f)"
          % (name, br, r0[0], r0[1], r0[2], r1[0], r1[1], r1[2]))
