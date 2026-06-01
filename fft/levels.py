#!/usr/bin/env python3 -B
"""
levels.py, supervision ladder: how much slower + better?
same bag64/leaf16/select protocol, swap ONLY the split rule:
 L0 random attr, random cut   (current)
 L1 random attr, median cut
 L2 random attr, best cut     (gini)
 L2k K random attrs, best cut (ExtraTrees; -k K)
 L3 all attrs, best cut       (CART-ish)
report ms/tree + selected test (recall prec fair) per dataset.

Options:
 -s --seed seed seed=1234567891
 -m --models models models=200
 -r --reps reps reps=5
 -n --N rows N=1000
 -k --K cand K=8
 -p --p exp p=2
"""
import random, math, time
from collections import Counter
import fft1
from fft1 import o, Data, clone, treeLeaf, confused, csv, Num

the = fft1.settings(__doc__); fft1.the = the
H = "/Users/timm/tmp/"
SETS = [("compas", H+"compas.csv", "race"),
        ("adult",  H+"adult.csv",  "race"),
        ("dutch",  H+"dutchf.csv", "sex"),
        ("diab",   H+"diab.csv",   "race")]

def Tree(): pass

def gini1(p): return 1 - p*p - (1-p)*(1-p)

def gimp(rows, lab):
  if not rows: return 0
  return gini1(sum(lab[id(r)] for r in rows)/len(rows))

def best_num(rows, c, lab, gpar, n):     # best <= cut on Num
  xs = sorted((r[c.at], lab[id(r)]) for r in rows if r[c.at] != "?")
  if len(xs) < 2: return None
  t1 = sum(y for _, y in xs); t0 = len(xs)-t1
  l1 = l0 = 0; best = None
  for i in range(len(xs)-1):
    l1 += xs[i][1]; l0 += 1-xs[i][1]
    if xs[i][0] == xs[i+1][0]: continue
    r1, r0 = t1-l1, t0-l0; nl, nr = l0+l1, r0+r1
    g = gpar - (nl*gini1(l1/nl) + nr*gini1(r1/nr))/n
    if best is None or g > best[0]: best = (g, xs[i][0])
  return None if best is None else (best[0], best[1], False)

def best_sym(rows, c, lab, gpar, n):     # best ==cat on Sym
  best = None
  for v in set(r[c.at] for r in rows if r[c.at] != "?"):
    ok = [r for r in rows if r[c.at] == v]
    no = [r for r in rows if r[c.at] != v]
    if not ok or not no: continue
    g = gpar - (len(ok)*gimp(ok, lab)+len(no)*gimp(no, lab))/n
    if best is None or g > best[0]: best = (g, v)
  return None if best is None else (best[0], best[1], True)

def best_on(rows, c, lab):
  n = len(rows); gpar = gimp(rows, lab)
  return (best_sym if c.it is not Num else best_num)(
    rows, c, lab, gpar, n)

def pick(rows, cols, lab, level):        # -> (c, v, sym)
  if level in ("L0", "L1"):
    c = random.choice(cols)
    vs = [r[c.at] for r in rows if r[c.at] != "?"]
    if not vs: return None
    if level == "L0":
      v = random.choice(vs)
    else:                                # median
      s = sorted(vs); v = s[len(s)//2]
    return c, v, c.it is not Num
  cand = cols if level == "L3" else \
         random.sample(cols, min(the.K, len(cols))) \
         if level == "L2k" else [random.choice(cols)]
  best = None
  for c in cand:
    b = best_on(rows, c, lab)
    if b and (best is None or b[0] > best[0]):
      best = (b[0], c, b[1], b[2])
  return None if best is None else (best[1], best[2], best[3])

def build(d, cols, lab, level, stop=16):
  def grow(rows):
    if len(rows) <= stop or gimp(rows, lab) == 0:
      return clone(d, rows)
    for _ in range(8):
      pk = pick(rows, cols, lab, level)
      if not pk: return clone(d, rows)
      c, v, sym = pk
      ok, no = [], []
      for r in rows:
        x = r[c.at]
        go = x != "?" and (x == v if sym else x <= v)
        (ok if go else no).append(r)
      if ok and no: break
    else:
      return clone(d, rows)
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

LEVELS = ["L0", "L1", "L2", "L2k", "L3"]

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
  res = {L: dict(t=0.0, ntree=0, picks=[]) for L in LEVELS}
  for _ in range(the.reps):
    rows = rows0[:]; random.shuffle(rows)
    n1, n2 = int(.6*len(rows)), int(.8*len(rows))
    fit, val, test = rows[:n1], rows[n1:n2], rows[n2:]
    base = clone(d, fit); cols = d.cols.x
    pos = [r for r in fit if lab[id(r)] == 1]
    neg = [r for r in fit if lab[id(r)] == 0]
    if not pos or not neg: continue
    for L in LEVELS:
      ms = []
      for _ in range(the.models):
        x   = random.randint(10, 90)
        nps = min(len(pos), round(64*x/100))
        bag = random.sample(pos, nps) + \
              random.sample(neg, min(len(neg), 64-nps))
        sub = clone(base, bag)
        t0 = time.process_time()
        t  = build(sub, cols, lab, L)
        res[L]["t"] += time.process_time()-t0
        res[L]["ntree"] += 1
        setVotes(t, lab); ms.append((sub, t))
      best = min(ms, key=lambda m: heaven(
        H3(m[1], val, lab, grp, groups)))
      res[L]["picks"].append(H3(best[1], test, lab, grp, groups))
  return res

if __name__ == "__main__":
  fft1.cli(the, __doc__); random.seed(the.seed)
  print("# ms/tree  + selected test (recall prec fair)  K=%d"
        % the.K)
  agg = {L: dict(t=0.0, ntree=0, picks=[]) for L in LEVELS}
  for name, file, g in SETS:
    res = run(name, file, g)
    print("\n# %s" % name)
    for L in LEVELS:
      r = res[L]; mspt = 1000*r["t"]/r["ntree"]
      av = tuple(sum(c)/len(c) for c in zip(*r["picks"]))
      print("  %-4s %6.2f ms  (%.2f %.2f %.2f) h=%.3f"
            % (L, mspt, av[0], av[1], av[2], heaven(av)))
      agg[L]["t"] += r["t"]; agg[L]["ntree"] += r["ntree"]
      agg[L]["picks"] += r["picks"]
  print("\n# ALL (mean over 4 sets)")
  base_ms = 1000*agg["L0"]["t"]/agg["L0"]["ntree"]
  for L in LEVELS:
    r = agg[L]; mspt = 1000*r["t"]/r["ntree"]
    av = tuple(sum(c)/len(c) for c in zip(*r["picks"]))
    print("  %-4s %6.2f ms  %4.1fx  (%.2f %.2f %.2f) h=%.3f"
          % (L, mspt, mspt/base_ms, av[0], av[1], av[2], heaven(av)))
