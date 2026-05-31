#!/usr/bin/env python3 -B
"""panels.py, run compas for several protected attrs -> one png.
usage: python3 panels.py [out.png] [group1 group2 ...]"""
import sys, subprocess
import matplotlib; matplotlib.use("Agg")
import matplotlib.pyplot as plt

out    = sys.argv[1] if len(sys.argv) > 1 else "/Users/timm/tmp/fair_all.png"
groups = sys.argv[2:] or ["race", "sex", "age_cat"]
trees  = "512"

panels = []                              # (title, recall, fair, prec)
for g in groups:
  txt = subprocess.run(["python3", "compas.py", "-t", trees, "-g", g],
                       capture_output=True, text=True).stdout
  name, rows = g, []
  for ln in txt.splitlines():
    if ln.startswith("#"): name = ln[1:].strip()
    elif ln.split():       rows.append(tuple(map(float, ln.split())))
  panels.append((name, [r[0] for r in rows], [r[1] for r in rows],
                 [r[3] for r in rows]))

allp = sorted(p for _,_,_,pr in panels for p in pr)
mid  = allp[len(allp)//2]                # shared colour centre

fig, axs = plt.subplots(1, len(panels), figsize=(5*len(panels), 4.6), sharey=True)
if len(panels) == 1: axs = [axs]
for ax, (name, rec, fair, prec) in zip(axs, panels):
  sc = ax.scatter(rec, fair, c=prec, cmap="RdBu_r", vmin=mid-.10, vmax=mid+.10,
                  s=22, alpha=.9, edgecolors="0.4", linewidths=.4)
  ax.scatter([1], [1], marker="*", s=200, color="black", zorder=5)
  ax.set_title("%s -- %d models" % (name, len(rec)))
  ax.set_xlabel("recall (pd)"); ax.set_xlim(0,1); ax.set_ylim(0,1); ax.grid(alpha=.3)
axs[0].set_ylabel("fairness (predictive equality)")
fig.colorbar(sc, ax=axs, format="%.2f",
             label="precision (white=%.2f median, red higher)" % mid)
import os; os.makedirs(os.path.dirname(out), exist_ok=True)
fig.savefig(out, dpi=120, bbox_inches="tight")
print("wrote", out)
