#!/usr/bin/env python3 -B
"""
plan.py: counterfactual planner. For a starting "here" leaf, yields
alternative leaves with lower y-values plus the x-column changes that
move "here" toward each.
"""
from core import *
from trees import treeNodes

def treePlan(tree, here):
  eps = the.stats.eps * spread(tree.ynum)
  for there, *_ in treeNodes(tree):
    if there.col is None:
      dy = mid(here.ynum) - mid(there.ynum)
      if dy > eps:
        diff = [f"{c.txt}={o(mid(c))}"
                for c, h in zip(there.d.cols.xs, here.d.cols.xs)
                if mid(c) != mid(h)]
        if diff: yield dy, mid(there.ynum), diff

__all__ = ["treePlan"]
