import unittest
from simp import Tab, pairs
from data import auto93, diabetes
from random import seed, choice


class TestSimp(unittest.TestCase):
  def rest_tab1(i):
    t = Tab(auto93)
    assert(len(t.cols.all) == 8)
    print(len(t.rows))
    print(t.rows[-1].bins)

  def rest_tab2(i):
    t = Tab(diabetes)
    t.bins('tested_positive')
    for row in t.rows:
      print(row.bins)

  def test_dom(i, n=20):
    t = Tab(auto93)
    t.bins(40)
    for r1 in t.rows:
      r1.dom = 0
      for _ in range(n):
        r1.dom += r1.better(choice(t.rows)) / n
    t.rows = sorted(t.rows, key=lambda r: r.dom)
    for row in t.rows[0::n]:
      print(row.status(), round(row.dom, 2))

  def test_pairs(i):
    for x, y in pairs([1, 2, 3, 4, 5, 6, 7, 8, 9]):
      print(x, y)

  def test_dom(i):
    t = Tab(auto93)
    t.bins()
    for c, b in t._bins.items():
      print(c, t.cols.all[c], len(b))
