#!/usr/bin/env python3 -B
"""
treeseg.py: tree library demos.
"""
from pathlib import Path
from core  import *
from trees import *

egopt = str(Path.home() / "gits/moot/optimize/misc/auto93.csv")

def test_h(): print(__doc__)

def test_tree(file=egopt):
  """Grow + show tree on train half."""
  _, d_train, _ = ready(file)
  treeShow(treeGrow(d_train, d_train.rows))

def test_funny(file=egopt):
  """Run test rows down tree, flag where actual disagrees with leaf."""
  d, d_train, test = ready(file)
  t = treeGrow(d_train, d_train.rows)
  for r in sorted(test, key=lambda r: disty(d_train, r))[:10]:
    lf  = treeLeaf(t, r)
    gap = disty(d_train, r) - mid(lf.ynum)
    flag = " !" if abs(gap) > spread(lf.ynum) else "  "
    print(f"{flag} actual={o(disty(d_train, r)):>5}"
          f"  leaf={o(mid(lf.ynum)):>5}"
          f"  gap={o(gap):>6}  n={lf.ynum.n}")

if __name__ == "__main__": cli()
