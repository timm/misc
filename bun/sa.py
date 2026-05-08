#!/usr/bin/env python3 -B
"""
sa.py: simulated annealing via (1+1) search.
"""
from core import *

def sa(d, oracle, restarts=0, m=0.5, budget=1000):
  n = max(1, int(m * len(d.cols.xs)))
  def accept(e, en, h, b):
    return en < e or rand() < exp((e - en) / (1 - h/b + 1e-32))
  def mutate(s): yield picks(d, s, n)
  return oneplus1(d, mutate, accept, oracle, budget, restarts)

__all__ = ["sa"]
