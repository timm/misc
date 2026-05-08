#!/usr/bin/env python3 -B
"""
bayes.py: incremental Naive Bayes (test-then-train) + confusion stats.
"""
from core import *

def classify(src, wait=10):
  src = iter(src)
  h, cf, all_ = {}, None, Data([next(src)])
  for n, row in enumerate(src):
    want = row[all_.cols.klass.at]
    if n >= wait:
      cf = dinc(want,
                max(h, key=lambda kl: likes(h[kl], row,
                                            len(all_.rows), len(h))),
                cf)
    if want not in h: h[want] = clone(all_)
    add(all_, add(h[want], row))
  return cf

def confused(cf):
  klasses = sorted(set(cf.keys()).union(
    {g for w in cf.values() for g in w.keys()}))
  total = sum(cf[w][g] for w in cf for g in cf[w])
  p = lambda y, z: int(100 * y / (z or 1e-32))
  out = []
  for c in klasses:
    tp = cf.get(c, {}).get(c, 0)
    fn = sum(cf.get(c, {}).values()) - tp
    fp = sum(cf.get(w, {}).get(c, 0) for w in cf if w != c)
    tn = total - tp - fn - fp
    pd, pr = p(tp, tp + fn), p(tp, fp + tp)
    sp = p(tn, tn + fp)
    out.append(S(tp=tp, fn=fn, fp=fp, tn=tn,
                 pd=pd, pr=pr,
                 f1=int(2*pd*pr / (pd + pr + 1e-32)),
                 g=int(2*pd*sp / (pd + sp + 1e-32)),
                 acc=p(tp + tn, total),
                 label="  " + c))
  return out

__all__ = ["classify", "confused"]
