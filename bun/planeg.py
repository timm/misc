#!/usr/bin/env python3 -B
"""
planeg.py: counterfactual plan demo (rung 3).
"""
from pathlib import Path
from core  import *
from trees import treeGrow, treeLeaf
from plan  import *

egopt = str(Path.home() / "gits/moot/optimize/misc/auto93.csv")

def test_h(): print(__doc__)

def test_plan(file=egopt):
  """Generate counterfactuals to improve worst row."""
  d, d_train, _ = ready(file)
  t = treeGrow(d_train, d_train.rows)
  here = treeLeaf(t, max(d.rows, key=lambda r: disty(d, r)))
  print(f"  now={o(mid(here.ynum))}")
  for dy, score, diff in sorted(treePlan(t, here)):
    print(f"  {o(score):>6} (dy={o(dy)}) if {', '.join(diff)}")

if __name__ == "__main__": cli()
