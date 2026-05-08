#!/usr/bin/env python3 -B
"""
acquireeg.py: active-learning demos.
"""
from pathlib import Path
from core    import *
from acquire import *
from trees   import treeGrow, treeLeaf, treeShow

egopt = str(Path.home() / "gits/moot/optimize/misc/auto93.csv")

def test_h(): print(__doc__)

def test_see(file=egopt):
  """Tree built from active-learned examples."""
  _, d_train, _ = ready(file)
  lab = acquire(d_train)
  treeShow(treeGrow(d_train, lab.rows))

def test_acquire(file=egopt):
  """Train/predict/score pipeline."""
  d0 = Data(csv(file))
  w1, w2 = Num(), Num()
  win = wins(d0)
  for _ in range(20):
    d, d_train, test_rows = ready(d0)
    lab = acquire(d_train)
    t   = treeGrow(d_train, lab.rows)
    guess = sorted(test_rows, key=lambda r: mid(treeLeaf(t, r).ynum))
    add(w1, win(min(lab.rows[:the.learn.check],
                    key=lambda r: disty(d_train, r))))
    add(w2, win(min(guess[:the.learn.check],
                    key=lambda r: disty(d_train, r))))
  print(f":budget {the.learn.budget} :check {the.learn.check} "
        f":train {int(mid(w1))} :test {int(mid(w2))}")

def test_acquire3(file=egopt):
  """Compare random vs Bayes vs centroid acquire."""
  d = Data(csv(file))
  W = wins(d); Y = lambda r: disty(d, r)
  out = {}
  for _ in range(20):
    shuffle(d.rows)
    n = len(d.rows) // 2
    test, train = d.rows[n:], d.rows[:n][:the.few]
    lab1 = train[:the.learn.budget]
    lab2 = acquire(clone(d, train))
    lab3 = acquire(clone(d, train), acquireWithCentroid)
    for how, lab in (("rand", lab1),
                     ("bayes", lab2.rows),
                     ("near",  lab3.rows)):
      d2    = clone(d, lab)
      tree  = treeGrow(d2, d2.rows)
      guess = sorted(test, key=lambda r: mid(treeLeaf(tree, r).ynum))
      out[how] = out.get(how) or Num()
      add(out[how], W(sorted(guess[:the.learn.check], key=Y)[0]))
  for how, num in out.items(): print(int(mid(num)), how, end=" ")
  print(the.learn.budget, "budget")

if __name__ == "__main__": cli()
