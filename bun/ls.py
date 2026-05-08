#!/usr/bin/env python3 -B
"""
ls.py: local search via (1+1) search (greedy accept, occasional bursts).
"""
from core import *

def ls(d, oracle, restarts=100, p=0.5, tries=20, budget=1000):
  def accept(e, en, *_): return en < e
  def mutate(s):
    c = choice(d.cols.xs)
    for _ in range(tries if rand() < p else 1):
      s = s[:]
      s[c.at] = pick(c, s[c.at])
      yield s
  return oneplus1(d, mutate, accept, oracle, budget, restarts)

__all__ = ["ls"]
