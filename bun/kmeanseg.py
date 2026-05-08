#!/usr/bin/env python3 -B
"""
kmeanseg.py: clustering benchmark.
"""
from pathlib import Path
from core   import *
from kmeans import *

egopt = str(Path.home() / "gits/moot/optimize/misc/auto93.csv")

def test_h(): print(__doc__)

def clustering(d0, build, near=1, fast=False):
  t_build, t_apply, err, repeats = 0, 0, 0, 10
  for _ in range(repeats):
    d, train, test = ready(d0)
    predict = lambda rs: (sum(disty(train, r) for r in rs) / len(rs)
                          if rs else 0)
    t_1 = now(); ds = build(train); t_2 = now()
    t_build += t_2 - t_1
    for r in test:
      near_rs = neighbors(train, r, ds, near=near, fast=fast)
      err += abs(disty(d, r) - predict(near_rs)) / len(test)
    t_apply += now() - t_2
  return [f"{x/repeats:>7.2f}"
          for x in (t_build/1e6, t_apply/1e6, err)]

def test_cluster(file=egopt):
  """Compare baseline / sample / rhalf / kmeans / kpp."""
  d = Data(csv(file)); k, near = 16, 1
  all_y = adds((disty(d, r) for r in d.rows), Num())
  results = []
  B  = lambda d1: [d1]
  S1 = lambda d1: [clone(d, sample(d.rows, 32))]
  RH = lambda d1: rhalf(d1, k=k)
  KM = lambda d1: kmeans(d1, k=k)
  KP = lambda d1: kmeans(d1, k=k, cents=kpp(d1, k=k))
  for txt, fn, fast in (("baseline", B, False), ("sample", S1, False),
                        ("rhalf", RH, False), ("kmeans", KM, False),
                        ("kpp", KP, False), ("rhalf f", RH, True),
                        ("kmeans f", KM, True), ("kpp f", KP, True)):
    t_build, t_apply, err = clustering(d, fn, near, fast=fast)
    results.append(dict(Algorithm=txt, T_Build=t_build,
                        T_Apply=t_apply, Err=err))
  print(f"Dataset small error threshold: {.35*spread(all_y):.2f}\n")
  table(results, w=12)

if __name__ == "__main__": cli()
