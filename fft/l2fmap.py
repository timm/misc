#!/usr/bin/env python3 -B
"""l2fmap.py, thesis-style figure for fmpick on compas:
LEFT  = cloud of 30 trees' val scores (run #0)
RIGHT = 20 runs' selected-on-val test scores
colored by precision; star at (1,1) = heaven."""
import random, math, os
import matplotlib; matplotlib.use("Agg")
import matplotlib.pyplot as plt
import fft1
from fft1 import (o, Data, clone, csv, confused,
                  fmtree, fmleaf, fmleaves)
import fmpick
fmpick.load()
d, lab, grp, groups = fmpick.d, fmpick.lab, fmpick.grp, fmpick.groups

random.seed(fft1.the.seed)
N = 4000
rows = d.rows[:]
if len(rows) > N: rows = random.sample(rows, N)

cloud, tests = [], []
for rep in range(20):
  random.shuffle(rows)
  n1, n2 = int(.6*len(rows)), int(.8*len(rows))
  train, val, test = rows[:n1], rows[n1:n2], rows[n2:]
  best, bestd = None, 1e9
  for _ in range(30):
    build = random.sample(train, min(64, len(train)))
    sub   = clone(d, build)
    t     = fmtree(sub, stop=8)
    pool  = random.sample(train, min(1024, len(train)))
    fmpick.setVotesFM(sub, t, pool)
    mv    = fmpick.metrics(sub, t, val)
    if rep == 0: cloud.append(mv)
    h = fmpick.heaven(mv)
    if h < bestd: bestd, best = h, (sub, t)
  tests.append(fmpick.metrics(best[0], best[1], test))

# precision range across both panels
allp = sorted(p for _,_,p in cloud + tests)
m   = allp[len(allp)//2]
lo, hi = m - 0.10, m + 0.10

fig, axs = plt.subplots(1, 2, figsize=(9, 4.4), sharex=True, sharey=True)
def draw(ax, pts, sz, title):
  rec  = [p[0] for p in pts]
  prec = [p[1] for p in pts]
  fair = [p[2] for p in pts]
  sc = ax.scatter(rec, fair, c=prec, cmap="RdBu_r",
                  vmin=lo, vmax=hi, s=sz, alpha=.8,
                  edgecolors="0.4", linewidths=.3)
  ax.scatter([1], [1], marker="*", s=110, color="black", zorder=5)
  ax.set_xlim(0, 1); ax.set_ylim(0, 1); ax.grid(alpha=.3)
  ax.set_title(title, fontsize=10)
  ax.set_xlabel("recall")
  return sc

sc = draw(axs[0], cloud, 8,  "CLOUD: 30 fmtrees (val, run #0)")
sc = draw(axs[1], tests, 50, "PICK: 20 runs best-on-val → test")
axs[0].set_ylabel("fairness (min/max FPR across race)")
fig.suptitle("fmpick on compas (K=64 build, Load=1024 vote, fmtree)")
fig.colorbar(sc, ax=axs, format="%.2f", fraction=.012,
             label="precision (white=%.2f, red higher)" % m)

out = "/Users/timm/gits/timm/misc/fft/img/l2fmap.pdf"
os.makedirs(os.path.dirname(out), exist_ok=True)
fig.savefig(out, bbox_inches="tight")
print("wrote", out)
