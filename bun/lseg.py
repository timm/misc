#!/usr/bin/env python3 -B
"""
lseg.py: local search demo + sa/ls comparison.
"""
from pathlib import Path
from core import *
from sa   import sa
from ls   import *

egopt = str(Path.home() / "gits/moot/optimize/misc/auto93.csv")

def test_h(): print(__doc__)

def test_ls(file=egopt):
  """Run LS on sample data."""
  d0 = Data(csv(file))
  shuffle(d0.rows)
  known  = clone(d0, d0.rows[:50])
  search = clone(d0, d0.rows[50:])
  oracle = lambda r: oracleNearest(known, r)
  print(f"{'evals':>6} {'energy':>7}")
  for h, e, row in ls(search, oracle):
    print(f"  {h:4}   {o(e):>5}")

def test_compare(file=egopt):
  """Compare sa-r, ls+r, ls-r, sa+r over 20 reps."""
  d0 = Data(csv(file))
  W = wins(d0); out = {}
  for _ in range(20):
    shuffle(d0.rows)
    known  = clone(d0, d0.rows[:50])
    search = clone(d0, d0.rows[50:])
    oracle = lambda r: oracleNearest(known, r)
    for name, fn in (("sa-r", lambda d: sa(d, oracle)),
                     ("ls+r", lambda d: ls(d, oracle)),
                     ("ls-r", lambda d: ls(d, oracle, restarts=0)),
                     ("sa+r", lambda d: sa(d, oracle, restarts=100))):
      _, _, r = last(fn(search))
      if name not in out: out[name] = Num(name)
      add(out[name], W(r))
  print(f"\n{file}")
  for k, v in sorted(out.items()):
    print(f"  {k:5} {o(mid(v)):>5} +/- {o(spread(v))}")

if __name__ == "__main__": cli()
