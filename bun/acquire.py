#!/usr/bin/env python3 -B
"""
acquire.py: active learning. Warm start, then iterated best/rest scoring.

Local options:
    --learn.start=4    initial labels
    --learn.budget=50  rows to evaluate
    --learn.check=5    guesses to check
"""
from core import *
loadDoc(__doc__)

def acquireWithBayes(data, best, rest, row):
  n = len(best.rows) + len(rest.rows)
  return likes(rest, row, n, 2) - likes(best, row, n, 2)

def acquireWithCentroid(data, best, rest, row):
  return distx(data, row, mids(best)) - distx(data, row, mids(rest))

def warm_start(data, rows, label):
  lab = clone(data, rows[:the.learn.start])
  lab.rows.sort(key=lambda r: disty(lab, label(data, r)))
  n = int(sqrt(len(lab.rows)))
  return (lab,
          clone(data, lab.rows[:n]),
          clone(data, lab.rows[n:]),
          rows[the.learn.start:])

def dont_let_Best_grow_too_big(best, rest, lab):
  if len(best.rows) > sqrt(len(lab.rows)):
    best.rows.sort(key=lambda r: disty(lab, r))
    rest.rows.append(
      add(rest.cols, sub(best.cols, best.rows.pop())))

def acquire(data, score=acquireWithCentroid,
            label=lambda _, row: row):
  rows = data.rows[:]
  shuffle(rows)
  lab, best, rest, unlab = warm_start(data, rows[:the.few], label)
  fn = lambda row: score(lab, best, rest, row)
  for _ in range(the.learn.budget):
    if not unlab: break
    pk, *unlab = sorted(unlab, key=fn)
    add(lab, add(best, label(data, pk)))
    dont_let_Best_grow_too_big(best, rest, lab)
  lab.rows.sort(key=lambda r: disty(lab, r))
  return lab

__all__ = ["acquireWithBayes", "acquireWithCentroid", "warm_start",
           "dont_let_Best_grow_too_big", "acquire"]
