#!/usr/bin/env python3 -B
"""
plot.py, scatter "acc fairness" pairs from stdin -> png.
usage: python3 fft1.py | python3 plot.py [out.png]
"""
import sys
import matplotlib; matplotlib.use("Agg")
import matplotlib.pyplot as plt

out = sys.argv[1] if len(sys.argv) > 1 else "/Users/timm/tmp/fair.png"
pts = [tuple(map(float, ln.split())) for ln in sys.stdin if ln.split()]
xs  = [p[0] for p in pts]; ys = [p[1] for p in pts]

def pareto(ps):                              # maximise both
  return sorted(p for p in ps
                if not any(q[0] >= p[0] and q[1] >= p[1] and q != p
                           for q in ps))
pf = pareto(pts)

plt.figure(figsize=(5, 5))
plt.scatter(xs, ys, s=10, alpha=.3, color="steelblue",
            label="%d models" % len(pts))
plt.plot([p[0] for p in pf], [p[1] for p in pf],
         "-o", color="crimson", ms=4, label="Pareto front")
plt.xlabel("Accuracy"); plt.ylabel("Fairness (predictive equality)")
plt.title("fastmap model cloud (held-out)")
plt.xlim(0, 1); plt.ylim(0, 1); plt.grid(alpha=.3); plt.legend()
import os; os.makedirs(os.path.dirname(out), exist_ok=True)
plt.tight_layout(); plt.savefig(out, dpi=120)
print("wrote", out, "(%d pts)" % len(pts))
