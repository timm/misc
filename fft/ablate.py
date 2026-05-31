#!/usr/bin/env python3 -B
"""
ablate.py, which diversification operators raise test dots?
build cloud of random-config models on 4 datasets, score each on
held-out test (heaven dist to recall=prec=fair=1, lower=better),
rank within dataset, then per operator: mean-rank by value +
top-half enrichment + build cost. tie -> drop (fix cheapest,
faster). winner -> keep.

Options:
 -s --seed   seed       seed=1234567891
 -m --models models/set models=500
 -r --reps   reps/set   reps=1
 -n --N      rows/set   N=1000
 -p --p      dist exp   p=2
"""
import random, math, time
from collections import Counter
import fft1
from fft1 import o, Data, clone, confused, csv, norm, Num

the = fft1.settings(__doc__); fft1.the = the

H = "/Users/timm/tmp/"
SETS = [                          # name, csv, protected attr
  ("compas", H+"compas.csv", "race"),
  ("adult",  H+"adult.csv",  "race"),
  ("dutch",  H+"dutchf.csv", "sex"),
  ("diab",   H+"diab.csv",   "race"),
]

OPS = dict(                       # operator menu (factors)
  split = ["axis", "radial"],     # cut mechanism
  cut   = ["row",  "mid"],        # cut point: row val vs median
  ratio = ["sweep","half"],       # class-bag x: 10..90 vs 50
  leaf  = ["sqrt", "4", "16"],    # leaf-size stop rule
  bag   = [64, 128, 256],         # bag size
  pp    = [1, 2],                 # minkowski for radial dist
  feat  = ["all", "sqrt"],        # feature subset
)

# ## tree (config-driven) --------------------------------------
def feats(d, cfg):
  xs = d.cols.x
  if cfg["feat"] == "all": return xs
  k = max(1, int(len(xs)**.5))
  return random.sample(xs, min(k, len(xs)))

def distx(cols, p, r1, r2):       # minkowski over x cols, normed
  s = n = 0
  for c in cols:
    a, b = r1[c.at], r2[c.at]
    if a == "?" and b == "?": continue
    if c.it is Num:
      A = norm(c, a) if a != "?" else None
      B = norm(c, b) if b != "?" else None
      if A is None: A = 0 if B > .5 else 1
      if B is None: B = 0 if A > .5 else 1
      inc = abs(A - B)
    else:
      inc = 0 if (a != "?" and a == b) else 1
    n += 1; s += inc**p
  return (s/n)**(1/p) if n else 0

def project(cols, p, a, b, r):    # cosine proj onto pole line
  ca = distx(cols, p, a, r); cb = distx(cols, p, b, r)
  ab = distx(cols, p, a, b) + 1e-32
  return (ca*ca + ab*ab - cb*cb) / (2*ab)

def stop_of(cfg, n0):
  return {"sqrt": n0**.5, "4": 4, "16": 16}[cfg["leaf"]]

def build(d, cols, cfg, stop):
  p = cfg["pp"]
  def grow(rows):
    if len(rows) <= stop: return clone(d, rows)
    for _ in range(10):
      if cfg["split"] == "radial":
        a, b = random.sample(rows, 2)
        pr = {id(r): project(cols, p, a, b, r) for r in rows}
        vs = sorted(pr.values())
        cut = vs[len(vs)//2] if cfg["cut"] == "mid" \
              else pr[id(random.choice(rows))]
        ok = [r for r in rows if pr[id(r)] <= cut]
        no = [r for r in rows if pr[id(r)] >  cut]
        node = o(it=Tree, split="radial", a=a, b=b,
                 cols=cols, p=p, cut=cut)
      else:
        c = random.choice(cols); sym = c.it is not Num
        if cfg["cut"] == "mid" and not sym:
          vals = sorted(r[c.at] for r in rows if r[c.at] != "?")
          if not vals: continue
          cut = vals[len(vals)//2]
        else:
          cut = random.choice(rows)[c.at]
          if cut == "?": continue
        ok, no = [], []
        for r in rows:
          x = r[c.at]
          go = x != "?" and (x == cut if sym else x <= cut)
          (ok if go else no).append(r)
        node = o(it=Tree, split="axis", at=c.at, sym=sym, cut=cut)
      if ok and no: break
    else:
      return clone(d, rows)
    node.left = grow(ok); node.right = grow(no)
    return node
  return grow(d.rows)

def Tree(): pass

def route(t, row):                # route row to leaf
  while t.it is Tree:
    if t.split == "radial":
      go = project(t.cols, t.p, t.a, t.b, row) <= t.cut
    else:
      x = row[t.at]
      go = x != "?" and (x == t.cut if t.sym else x <= t.cut)
    t = t.left if go else t.right
  return t

# ## scoring ---------------------------------------------------
def leaves(t):
  if t.it is fft1.Data: yield t
  else: yield from leaves(t.left); yield from leaves(t.right)

def setVotes(t, lab):
  for lf in leaves(t):
    s = sum(lab[id(r)] for r in lf.rows)
    lf.vote = int(2*s > len(lf.rows))

def metrics(t, rows, lab, grp, groups):
  pairs = []; gp = {g: [] for g in groups}
  for r in rows:
    got = route(t, r).vote
    pairs.append((lab[id(r)], got))
    if grp[id(r)] in groups: gp[grp[id(r)]].append((lab[id(r)], got))
  c   = confused(pairs).get(1, o(pd=0, prec=0))
  fpr = [confused(g).get(1, o(pf=0)).pf for g in gp.values()]
  return c.pd, c.prec, min(fpr)/(max(fpr)+1e-32)

def heaven(m): return math.sqrt(sum((1-v)**2 for v in m))

def bagOf(pos, neg, cfg):
  sz = cfg["bag"]
  x  = random.randint(10, 90) if cfg["ratio"] == "sweep" else 50
  nps = min(len(pos), round(sz*x/100))
  return random.sample(pos, nps) + \
         random.sample(neg, min(len(neg), sz-nps))

# ## per-dataset run -------------------------------------------
def run_set(name, file, gattr):
  d = Data(list(csv(file)))
  yat = next(c.at for c in d.cols.all if str(c.txt).endswith("!"))
  gat = next(c.at for c in d.cols.all if c.txt == gattr)
  lab = {id(r): (0 if r[yat] in ("?", "") else int(r[yat]))
         for r in d.rows}
  grp = {id(r): r[gat] for r in d.rows}
  groups = [g for g, _ in Counter(grp.values()).most_common(2)]
  base_rows = d.rows[:]
  if len(base_rows) > the.N:
    base_rows = random.sample(base_rows, the.N)
  ms = []
  for _ in range(the.reps):
    rows = base_rows[:]; random.shuffle(rows)
    n1, n2 = int(.6*len(rows)), int(.8*len(rows))
    fit, test = rows[:n1], rows[n2:]            # val unused here
    base = clone(d, fit)
    pos = [r for r in fit if lab[id(r)] == 1]
    neg = [r for r in fit if lab[id(r)] == 0]
    if not pos or not neg: continue
    for _ in range(the.models):
      cfg  = {k: random.choice(v) for k, v in OPS.items()}
      cols = feats(base, cfg)
      sub  = clone(base, bagOf(pos, neg, cfg))
      if not sub.rows: continue
      stop = stop_of(cfg, len(sub.rows))
      t0   = time.process_time()
      t    = build(sub, cols, cfg, stop)
      bt   = time.process_time() - t0
      setVotes(t, lab)
      sc   = heaven(metrics(t, test, lab, grp, groups))
      ms.append(o(set=name, cfg=cfg, score=sc, bt=bt))
  order = sorted(range(len(ms)), key=lambda i: ms[i].score)
  for r, i in enumerate(order):
    ms[i].rank = r/(len(ms)-1) if len(ms) > 1 else 0
  return ms

# ## analysis --------------------------------------------------
def mean(xs): xs = list(xs); return sum(xs)/len(xs) if xs else 0

if __name__ == "__main__":
  fft1.cli(the, __doc__); random.seed(the.seed)
  allm = []
  for name, file, g in SETS:
    t0 = time.time()
    ms = run_set(name, file, g); allm += ms
    print("# %-7s %d models  %.1fs  best=%.3f"
          % (name, len(ms), time.time()-t0,
             min((m.score for m in ms), default=0)))
  print("\n# OPERATOR ABLATION  (mean-rank: 0=best,1=worst)")
  print("# top = frac of this value in top-half (0.50=neutral)")
  drops, keeps = [], []
  for f in OPS:
    vals = OPS[f]; rows = []
    for v in vals:
      sub = [m for m in allm if m.cfg[f] == v]
      if not sub: continue
      mr  = mean(m.rank for m in sub)
      top = mean(1 if m.rank < .5 else 0 for m in sub)
      bt  = mean(m.bt for m in sub)*1000
      rows.append((v, mr, top, bt, len(sub)))
    rows.sort(key=lambda r: r[1])
    delta = rows[-1][1] - rows[0][1]
    best  = rows[0][0]
    verdict = "DROP fix=%s" % best if delta < .03 \
              else "KEEP best=%s" % best
    (drops if delta < .03 else keeps).append((f, best))
    print("\n%-6s  delta=%.3f  -> %s" % (f, delta, verdict))
    for v, mr, top, bt, n in rows:
      print("  %-7s rank=%.3f top=%.2f bt=%.2fms n=%d"
            % (v, mr, top, bt, n))
  print("\n# SUMMARY")
  print("# KEEP (real diversifiers):",
        ", ".join("%s=%s" % k for k in keeps) or "none")
  print("# DROP (tie -> fix, faster):",
        ", ".join("%s=%s" % k for k in drops) or "none")
  bt_ax = mean(m.bt for m in allm if m.cfg["split"]=="axis")*1000
  bt_ra = mean(m.bt for m in allm if m.cfg["split"]=="radial")*1000
  print("# build ms: axis=%.2f radial=%.2f (%.1fx)"
        % (bt_ax, bt_ra, bt_ra/(bt_ax+1e-32)))
