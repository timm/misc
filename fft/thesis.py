#!/usr/bin/env python3 -B
"""
thesis.py, the headline figure for ONE or MORE datasets in one png:
 LEFT  col = model cloud (space of options, train)
 RIGHT col = heaven-selected models (result of reasoning, test)
one row per dataset x protected-attr, coloured by precision.

usage: python3 thesis.py KEY [KEY ...]      (keys below)
       python3 thesis.py FILE attr1 attr2   (one ad-hoc dataset)
       -> writes ~/tmp/thesis_<keys>.png
"""
import sys, os, subprocess
import matplotlib; matplotlib.use("Agg")
import matplotlib.pyplot as plt

H = "/Users/timm/tmp/"
SETS = {  # key -> (csv, [protected attrs])
  "compas": (H+"compas.csv", ["race", "sex", "age_cat"]),
  "adult":  (H+"adult.csv",  ["race", "gender", "agegrp"]),
  "dutch":  (H+"dutchf.csv", ["sex", "Country_birth", "agegrp"]),
  "diab":   (H+"diab.csv",   ["race", "gender", "age"]),
}
ARGS = ["-m", "800", "-n", "2000", "-v", "200", "-r", "20"]
# cheap models (bag64) -> afford big N (stable fairness); -v caps val

args = sys.argv[1:] or ["compas", "adult", "dutch", "diab"]
if args[0] in SETS:                       # dataset keys -> rows
  dsets = [(k, SETS[k][0], SETS[k][1]) for k in args]
  stem  = "_".join(args)
else:                                     # FILE attr attr ... (one row)
  dsets = [(os.path.basename(args[0]).split(".")[0], args[0], args[1:])]
  stem  = dsets[0][0]
out = H + "thesis_%s.png" % stem

def run(file, g):                         # -> cloud, test
  txt = subprocess.run(["python3", "pick.py", "-f", file, "-g", g]+ARGS,
                       capture_output=True, text=True).stdout
  cloud, test = [], []
  for ln in txt.splitlines():
    p = ln.split()
    if   p and p[0]=="CLOUD": cloud.append(tuple(map(float, p[1:])))
    elif p and p[0]=="TEST":  test.append(tuple(map(float, p[1:])))
  return cloud, test                      # (recall, fair, prec)

cells = [[(g,)+run(f, g) for g in attrs] for _,f,attrs in dsets]
allp  = sorted(r[2] for row in cells for _,c,t in row for r in c+t)
m = allp[len(allp)//2]; lo, hi = m-0.10, m+0.10

na = max(len(r) for r in cells)           # attrs per dataset
nr, nc = len(cells), 2*na                 # 1 row/dataset; cloud|test blocks
fig, axs = plt.subplots(nr, nc, figsize=(2.3*nc, 2.4*nr),
                        sharex=True, sharey=True, squeeze=False)
sc = None
def draw(ax, pts, sz):
  global sc
  rec=[p[0] for p in pts]; fair=[p[1] for p in pts]; prec=[p[2] for p in pts]
  sc = ax.scatter(rec, fair, c=prec, cmap="RdBu_r", vmin=lo, vmax=hi,
                  s=sz, alpha=.85, edgecolors="0.4", linewidths=.3)
  ax.scatter([1],[1], marker="*", s=90, color="black", zorder=5)
  ax.set_xlim(0,1); ax.set_ylim(0,1); ax.grid(alpha=.3)
for i, row in enumerate(cells):
  for j, (g, cloud, test) in enumerate(row):
    draw(axs[i][j],    cloud, 6)          # left block = cloud
    draw(axs[i][j+na], test, 35)          # right block = selected/test
    if i==0:
      axs[0][j].set_title("cloud: %s" % g, fontsize=8)
      axs[0][j+na].set_title("pick: %s" % g, fontsize=8)
  axs[i][0].set_ylabel(dsets[i][0], fontsize=9, weight="bold")
fig.text(0.30, 0.995, "SPACE OF OPTIONS (train)",
         ha="center", weight="bold")
fig.text(0.73, 0.995, "RESULT OF REASONING (test)",
         ha="center", weight="bold")
for ax in axs[-1]: ax.set_xlabel("recall")
fig.colorbar(sc, ax=axs, format="%.2f", fraction=.012,
             label="precision (white=%.2f, red higher)" % m)
fig.savefig(out, dpi=120, bbox_inches="tight")
print("wrote", out)
