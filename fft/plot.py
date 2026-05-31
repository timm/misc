#!/usr/bin/env python3 -B
"""
plot.py, model cloud from stdin -> png.
each stdin line: recall fair pf prec acc
x=recall, y=fairness (heaven = top-right), colour=pf (green=low).
usage: python3 compas.py | python3 plot.py [out.png]
"""
import sys
import matplotlib; matplotlib.use("Agg")
import matplotlib.pyplot as plt

out    = sys.argv[1] if len(sys.argv) > 1 else "/Users/timm/tmp/fair.png"
name   = "data"; rows = []
for ln in sys.stdin:
  if ln.startswith("#"): name = ln[1:].strip()
  elif ln.split():       rows.append(tuple(map(float, ln.split())))
recall = [r[0] for r in rows]; fair = [r[1] for r in rows]
pf     = [r[2] for r in rows]; prec = [r[3] for r in rows]

plt.figure(figsize=(6, 5))
# precision colour window: fixed via argv[2] argv[3], else median +/-0.10
if len(sys.argv) > 3:
  lo, hi = float(sys.argv[2]), float(sys.argv[3])
else:
  m = sorted(prec)[len(prec)//2]; lo, hi = m-0.10, m+0.10
sc = plt.scatter(recall, fair, c=prec, cmap="RdBu_r", vmin=lo, vmax=hi,
                 s=26, alpha=.9, edgecolors="0.4", linewidths=.4)
plt.colorbar(sc, extend="both", format="%.2f",
             label="precision (white=%.2f, red higher)" % ((lo+hi)/2))
plt.scatter([1], [1], marker="*", s=250, color="black", zorder=5)  # heaven
plt.annotate("heaven", (1, 1), textcoords="offset points", xytext=(-55, -4))
import os
plt.xlabel("recall (pd)"); plt.ylabel("fairness (predictive equality)")
plt.title("%s -- %d models" % (name, len(rows)))
plt.xlim(0, 1); plt.ylim(0, 1); plt.grid(alpha=.3)
import os; os.makedirs(os.path.dirname(out), exist_ok=True)
plt.tight_layout(); plt.savefig(out, dpi=120)
print("wrote", out, "(%d models)" % len(rows))
