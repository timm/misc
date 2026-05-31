#!/usr/bin/env python3 -B
"""grid.py, run fairml over datasets x protected-attrs -> one figure.
rows = datasets, cols = protected attributes. colour = precision.
usage: python3 grid.py [out.png]"""
import sys, subprocess
import matplotlib; matplotlib.use("Agg")
import matplotlib.pyplot as plt

out  = sys.argv[1] if len(sys.argv) > 1 else "/Users/timm/tmp/fair_grid.png"
SPEC = [("/Users/timm/tmp/adult.csv",  ["gender", "race", "agegrp"]),
        ("/Users/timm/tmp/dutchf.csv", ["sex", "Country_birth", "agegrp"]),
        ("/Users/timm/tmp/kdd.csv",    ["sex", "race", "agegrp"])]
TREES = "512"

def run(f, g):
  txt = subprocess.run(["python3", "fairml.py", "-f", f, "-g", g, "-t", TREES],
                       capture_output=True, text=True).stdout
  name, rows = g, []
  for ln in txt.splitlines():
    if ln.startswith("#"): name = ln[1:].strip()
    elif ln.split():       rows.append(tuple(map(float, ln.split())))
  return name, rows

cells = [[run(f, g) for g in gs] for f, gs in SPEC]
allp  = sorted(r[3] for row in cells for _, rows in row for r in rows)
mid   = allp[len(allp)//2]

nr, nc = len(cells), max(len(r) for r in cells)
fig, axs = plt.subplots(nr, nc, figsize=(4.2*nc, 3.8*nr), sharex=True, sharey=True)
sc = None
for i, row in enumerate(cells):
  for j, (name, rows) in enumerate(row):
    ax = axs[i][j]
    rec=[r[0] for r in rows]; fair=[r[1] for r in rows]; prec=[r[3] for r in rows]
    sc = ax.scatter(rec, fair, c=prec, cmap="RdBu_r", vmin=mid-.1, vmax=mid+.1,
                    s=14, alpha=.85, edgecolors="0.4", linewidths=.3)
    ax.scatter([1],[1], marker="*", s=160, color="black", zorder=5)
    ax.set_title(name, fontsize=10)
    ax.set_xlim(0,1); ax.set_ylim(0,1); ax.grid(alpha=.3)
    if j==0: ax.set_ylabel("fairness")
    if i==nr-1: ax.set_xlabel("recall")
fig.colorbar(sc, ax=axs, format="%.2f",
             label="precision (white=%.2f median, red higher)" % mid)
import os; os.makedirs(os.path.dirname(out), exist_ok=True)
fig.savefig(out, dpi=120, bbox_inches="tight")
print("wrote", out)
