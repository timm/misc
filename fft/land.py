#!/usr/bin/env python3 -B
"""land.py, one landscape png:
 LEFT  half = the model cloud (space of options, all models on train)
 RIGHT half = the selected models on held-out test (result of reasoning)
rows = datasets, 3 protected attrs per half. usage: python3 land.py [out]"""
import sys, subprocess
import matplotlib; matplotlib.use("Agg")
import matplotlib.pyplot as plt

out  = sys.argv[1] if len(sys.argv) > 1 else "/Users/timm/tmp/land.png"
SPEC = [("/Users/timm/tmp/compas.csv", ["race", "sex", "age_cat"]),
        ("/Users/timm/tmp/adult.csv",  ["race", "gender", "agegrp"]),
        ("/Users/timm/tmp/dutchf.csv", ["sex", "Country_birth", "agegrp"]),
        ("/Users/timm/tmp/diab.csv",   ["race", "gender", "age"])]
ARGS = ["-t", "60", "-n", "1000", "-r", "12"]

def run(f, g):
  txt = subprocess.run(["python3", "pick.py", "-f", f, "-g", g]+ARGS,
                       capture_output=True, text=True).stdout
  name, cloud, test = g, [], []
  for ln in txt.splitlines():
    p = ln.split()
    if   ln.startswith("#"):    name = ln[1:].strip()
    elif p and p[0]=="CLOUD":   cloud.append(tuple(map(float, p[1:])))
    elif p and p[0]=="TEST":    test.append(tuple(map(float, p[1:])))
  return name, cloud, test                     # (recall, fair, prec)

cells = [[run(f, g) for g in gs] for f, gs in SPEC]
allp  = sorted(r[2] for row in cells for _,c,t in row for r in c+t)
m = allp[len(allp)//2]; lo, hi = m-0.10, m+0.10

nr, ng = len(cells), 3
fig, axs = plt.subplots(nr, 2*ng, figsize=(3.0*2*ng, 3.0*nr),
                        sharex=True, sharey=True)
sc = None
def draw(ax, pts, sz):
  global sc
  rec=[p[0] for p in pts]; fair=[p[1] for p in pts]; prec=[p[2] for p in pts]
  sc = ax.scatter(rec, fair, c=prec, cmap="RdBu_r", vmin=lo, vmax=hi,
                  s=sz, alpha=.8, edgecolors="0.4", linewidths=.3)
  ax.scatter([1],[1], marker="*", s=110, color="black", zorder=5)
  ax.set_xlim(0,1); ax.set_ylim(0,1); ax.grid(alpha=.3)
for i, row in enumerate(cells):
  for j, (name, cloud, test) in enumerate(row):
    draw(axs[i][j],      cloud, 8)              # left: space of options
    draw(axs[i][j+ng],   test, 45)             # right: result of reasoning
    axs[i][j].set_title(name, fontsize=9)
    axs[i][j+ng].set_title(name, fontsize=9)
    if j==0: axs[i][0].set_ylabel(cloud and name.split(" / ")[0] or "")
for ax in axs[-1]: ax.set_xlabel("recall")
fig.text(0.30, 0.995, "SPACE OF OPTIONS (cloud, train)", ha="center", weight="bold")
fig.text(0.74, 0.995, "RESULT OF REASONING (selected, test)", ha="center", weight="bold")
fig.colorbar(sc, ax=axs, format="%.2f", fraction=.02,
             label="precision (white=%.2f, red higher)" % m)
import os; os.makedirs(os.path.dirname(out), exist_ok=True)
fig.savefig(out, dpi=120, bbox_inches="tight"); print("wrote", out)
