#!/usr/bin/env python3 -B
"""
tree.py, experimental tree built from fft1's engine, but the node
split tries ALL x-cols (best global cut = CART-ish), not one random
col. Kept separate so the live fft1.tree (random col) is untouched.
Next step: feed it Fastmap dims instead of raw cols (dims:trees).

Same histogram cut as fft1: binOf -> Num(yfun) per bin, sweep with
merge/unmerge for the min child-variance (L.m2+R.m2) cut.
"""
import random
from functools import reduce
import fft1
from fft1 import (o, Num, Sym, Data, Tree, add, clone, merge,
                  binOf, cutgo, treeLeaf, leaves, BIG, csv)

def tree(root, stop=None, yfun=None, bins=7):
  yfun = yfun or (lambda r: r[root.cols.klass.at])
  stop = stop or fft1.the.stop or len(root.rows)**.5

  def cuts(c, rows):                 # yield (impurity, cut-value) over bins
    hist = {}
    for r in rows:
      if (x := r[c.at]) != "?":
        add(hist.setdefault(binOf(c, x, bins), Num()), yfun(r))
    bs = sorted(hist.items())
    if not bs: return
    total = reduce(merge, (v for _, v in bs))
    if c.it is Sym:
      for k, v in bs:
        R = merge(total, v, -1)
        if v.n and R.n: yield v.m2 + R.m2, k
    else:
      L = Num()
      for k, v in bs[:-1]:
        L = merge(L, v)
        R = merge(total, L, -1)
        if L.n and R.n: yield L.m2 + R.m2, k

  def grow(rows):
    if len(rows) <= stop: return clone(root, rows)
    best, bc, bv = BIG, None, "?"      # best cut across ALL cols
    for c in root.cols.x:
      for imp, v in cuts(c, rows):
        if imp < best: best, bc, bv = imp, c, v
    if bc is None: return clone(root, rows)   # no valid cut -> leaf
    ok, no = [], []
    for r in rows:
      (ok if cutgo(bc, r[bc.at], bv) else no).append(r)
    if not (ok and no): return clone(root, rows)
    return o(it=Tree, at=bc.at, cut=bv, left=grow(ok), right=grow(no))

  return grow(root.rows)

if __name__ == "__main__":           # smoke: leaves + depth on -f
  fft1.cli(fft1.the, fft1.__doc__); random.seed(fft1.the.seed)
  d = Data(list(csv(fft1.the.file)))
  t = tree(d, 16)
  depth = lambda t: 0 if t.it is Data else \
                    1 + max(depth(t.left), depth(t.right))
  print("leaves", sum(1 for _ in leaves(t)), "depth", depth(t))
