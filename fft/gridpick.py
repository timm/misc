#!/usr/bin/env python3 -B
"""gridpick.py, train/select/test over datasets x protected attrs.
Two grids (train, test), shared precision colour. rows=datasets,
cols=protected attrs.  usage: python3 gridpick.py"""
import subprocess
import matplotlib; matplotlib.use("Agg")
import matplotlib.pyplot as plt

SPEC = [("/Users/timm/tmp/compas.csv", ["race", "sex", "age_cat"]),
        ("/Users/timm/tmp/adult.csv",  ["race", "gender", "agegrp"]),
        ("/Users/timm/tmp/dutchf.csv", ["sex", "Country_birth", "agegrp"]),
        ("/Users/timm/tmp/diab.csv",   ["race", "gender", "age"])]
ARGS = ["-t", "60", "-n", "1000", "-r", "12"]

def run(f, g):
  txt = subprocess.run(["python3", "pick.py", "-f", f, "-g", g]+ARGS,
                       capture_output=True, text=True).stdout
  name, tr, te = g, [], []
  for ln in txt.splitlines():
    p = ln.split()
    if   ln.startswith("#"):     name = ln[1:].strip()
    elif p and p[0]=="TRAIN":    tr.append(tuple(map(float, p[1:])))
    elif p and p[0]=="TEST":     te.append(tuple(map(float, p[1:])))
  return name, tr, te                      # rows: (recall, fair, prec)

cells = [[run(f, g) for g in gs] for f, gs in SPEC]
allp  = sorted(r[2] for row in cells for _,tr,te in row for r in tr+te)
m     = allp[len(allp)//2]; lo, hi = m-0.10, m+0.10

def grid(which, out):                      # which: index 1=train,2=test of cell
  nr, nc = len(cells), max(len(r) for r in cells)
  fig, axs = plt.subplots(nr, nc, figsize=(4.2*nc, 3.8*nr), sharex=True, sharey=True)
  sc = None
  for i, row in enumerate(cells):
    for j, cell in enumerate(row):
      name, pts = cell[0], cell[which]
      ax = axs[i][j]
      rec=[p[0] for p in pts]; fair=[p[1] for p in pts]; prec=[p[2] for p in pts]
      sc = ax.scatter(rec, fair, c=prec, cmap="RdBu_r", vmin=lo, vmax=hi,
                      s=40, alpha=.9, edgecolors="0.3", linewidths=.5)
      ax.scatter([1],[1], marker="*", s=160, color="black", zorder=5)
      ax.set_title(name, fontsize=10)
      ax.set_xlim(0,1); ax.set_ylim(0,1); ax.grid(alpha=.3)
      if j==0: ax.set_ylabel("fairness")
      if i==nr-1: ax.set_xlabel("recall")
  fig.suptitle(out.split("/")[-1], y=1.0)
  fig.colorbar(sc, ax=axs, format="%.2f",
               label="precision (white=%.2f, red higher)" % m)
  import os; os.makedirs(os.path.dirname(out), exist_ok=True)
  fig.savefig(out, dpi=120, bbox_inches="tight"); print("wrote", out)

grid(1, "/Users/timm/tmp/grid_train.png")
grid(2, "/Users/timm/tmp/grid_test.png")
