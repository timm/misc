#!/usr/bin/env python3 -B
"""
kmeans.py: k-means, k-means++, recursive halving, neighbor lookup.
"""
from core import *

def kmeans(d, rs=None, k=10, n=10, cents=None):
  rs, out = rs or d.rows, []
  cents = cents or choices(rs, k=k)
  for _ in range(n):
    out = [clone(d) for _ in cents]
    for r in rs:
      add(out[min(range(len(cents)),
                  key=lambda j: distx(d, cents[j], r))], r)
    cents = [mids(kid) for kid in out if kid.rows]
  return out

def kpp(d, rs=None, k=10, few=256):
  rs = rs or d.rows
  out = [choice(rs)]
  while len(out) < k:
    t = sample(rs, min(few, len(rs)))
    ws = {i: min(distx(d, t[i], c)**2 for c in out)
          for i in range(len(t))}
    out.append(t[pick(ws)])
  return out

def half(d, rs, few=20):
  t = sample(rs, min(few, len(rs)))
  gap, east, west = max(((distx(d, r1, r2), r1, r2)
                          for r1 in t for r2 in t),
                        key=lambda z: z[0])
  proj = lambda r: (distx(d, r, east)**2 + gap**2
                    - distx(d, r, west)**2) / (2*gap + 1e-32)
  rs = sorted(rs, key=proj)
  n = len(rs) // 2
  return rs[:n], rs[n:], east, west, gap, proj(rs[n])

def rhalf(d, rs=None, k=10, stop=None, few=20):
  rs   = rs   if rs is not None else d.rows
  stop = stop or 20
  if len(rs) <= 2*stop: return [clone(d, rs)]
  l, r, *_ = half(d, rs, few)
  return rhalf(d, l, k, stop, few) + rhalf(d, r, k, stop, few)

def neighbors(d, r1, ds, near=1, fast=False):
  c = min(ds, key=lambda c: distx(d, r1, mids(c)))
  return ([mids(c)] if fast
          else sorted(c.rows, key=lambda r2: distx(d, r1, r2))[:near])

__all__ = ["kmeans", "kpp", "half", "rhalf", "neighbors"]
