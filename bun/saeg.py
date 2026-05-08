#!/usr/bin/env python3 -B
"""
saeg.py: simulated annealing demo.
"""
from pathlib import Path
from core import *
from sa   import *

egopt = str(Path.home() / "gits/moot/optimize/misc/auto93.csv")

def test_h(): print(__doc__)

def test_sa(file=egopt):
  """Run SA on sample data."""
  d0 = Data(csv(file))
  shuffle(d0.rows)
  known  = clone(d0, d0.rows[:50])
  search = clone(d0, d0.rows[50:])
  oracle = lambda r: oracleNearest(known, r)
  print(f"{'evals':>6} {'energy':>7}")
  for h, e, row in sa(search, oracle):
    print(f"  {h:4}   {o(e):>5}")

if __name__ == "__main__": cli()
