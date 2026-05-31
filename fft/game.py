#!/usr/bin/env python3 -B
"""game.py, ablate compas knobs: maximise recall-fairness AUC,
minimise #models, #mutations, runtime."""
import time, random
import fft1, compas
from fft1 import o, Data, clone, rmap
from collections import Counter

def auc(pts):                      # ROC AUC: pts=(pf,recall), heaven=top-left
  nd = [p for p in pts if not any(q[1]>=p[1] and q[0]<=p[0] and q!=p for q in pts)]
  nd.sort()                        # by pf ascending
  xs = [0]+[p[0] for p in nd]+[1]; ys = [0]+[p[1] for p in nd]+[1]
  return sum((xs[i+1]-xs[i])*(ys[i+1]+ys[i])/2 for i in range(len(xs)-1))

def run(n, ratio=1, featbag=1, leaf=1, pp=1, bag=128):
  random.seed(1)
  names, rows, lab, grp = compas.features()
  random.shuffle(rows)
  cut = int(.8*len(rows)); pool, test = rows[:cut], rows[cut:]
  base   = Data([names]+pool)
  pos    = [r for r in pool if lab[id(r)]==1]
  neg    = [r for r in pool if lab[id(r)]==0]
  groups = [g for g,_ in Counter(grp.values()).most_common(2)]
  allx   = base.cols.x
  pts, t = [], time.time()
  for _ in range(n):
    if ratio:
      x = random.randint(10,90); npos = min(len(pos), round(bag*x/100))
      smp = random.sample(pos, npos) + random.sample(neg, min(len(neg), bag-npos))
    else:
      smp = random.sample(pool, min(bag, len(pool)))
    sub = clone(base, smp)
    sub.cols.x = random.sample(allx, random.randint(2,len(allx))) if featbag else allx
    stop  = random.randint(2,int(len(sub.rows)**.5)) if leaf else int(len(sub.rows)**.5)
    fft1.the.p = random.uniform(.5,4) if pp else 2
    r = compas.evaluate(sub, rmap(sub, stop), test, lab, grp, groups)
    pts.append((r[2], r[0]))       # (pf, recall)
  return auc(pts), time.time()-t

if __name__ == "__main__":
  print("%-34s %6s %7s" % ("config", "AUC", "secs"))
  def show(tag, **kw):
    a, s = run(**kw); print("%-34s %6.3f %7.2f" % (tag, a, s))
  # knee sweep: ratio + featbag (featbag = speed), vary n
  for n in (30, 60, 120, 250, 500, 1000):
    show("ratio+featbag n=%d" % n, n=n, leaf=0, pp=0)
