"""
Trying something simple.
"""

import unittest
from random import random, seed
from data import auto93


class o:
  "Class that can pretty print."
  def __init__(i, **d):
    i.__dict__.update(**d)

  def __repr__(i):
    d, n = i.__dict__, i.__class__.__name__
    return n + "{" + ', '.join(
        [(':%s %s' % (k, d[k]))
         for k in sorted(d.keys())
         if str(k)[0] != "_"
         ]) + "}"


class Some(o):
  "Collect some examples, not all."
  hi = 256  # max number of items to collect
  magic = 2.54  # sd = (90th - 10th)/magic

  def __init__(i, pos=0, txt=" ", inits=[]):
    i.pos, i.txt, i.n, i._all, i.sorted = pos, txt, 0, [], False
    i.w = -1 if txt[0] == Tab.less else 1
    i.nump = txt[0] in Tab.nums
    [i.add(x) for x in inits]

  def add(i, x):
    if x != Tab.skip:
      if len(i._all) < Some.hi:
        i.sorted = False
        i.n += 1
        i._all += [x]
      elif random() < Some.hi / i.n:
        i.sorted = False
        i.n += 1
        i._all[int(len(i._all) * random())] = x
    return x

  def all(i):
    if not i.sorted:
      i._all = sorted(i._all)
      i.sorted = True
    return i._all

  def per(i, p=.5, lo=None, hi=None):
    lo = lo or 0
    hi = hi or len(i._all)
    n = lo + (hi - lo) * p
    return i.all()[int(n)]

  def sd(i, lo=None, hi=None):
    return (i.per(.9, lo=lo, hi=hi) -
            i.per(.1, lo=lo, hi=hi)) / Some.magic


class Bins:
  cohen = 0.3  # trivial difference = cohen*sd
  enough = 0.5  # min size of each bin is size**enough

  def __init__(i, some):
    i.some = some
    i.enough = len(some._all)**Bins.enough
    i.tiny = Bins.cohen * some.sd()
    i.splits = i.split(some.all())
    i.all = [some.all()[x] for x, _ in i.bins(i.splits)]

  def split(i, a):
    n1, lst = 0, [[0, 0]]
    for n2, x in enumerate(a):
      if n2 - n1 >= i.enough:  # enough here
        if len(a) - n2 >= i.enough:  # enough after
          if a[n2] != a[n2 - 1]:  # we can split here
            lst += [[n2, n2]]
            n1 = n2
      lst[-1][1] = n2 + 1
    return lst

  def bins(i, b4):
    j, after = 0, []
    while j < len(b4):
      a = b4[j]
      if j < len(b4) - 1:
        b = b4[j + 1]
        if i.bothIsBetter(a, b):
          a = [a[0], b[1]]
          j += 1
      after += [a]
      j += 1
    return i.bins(after) if len(after) < len(b4) else after

  def bothIsBetter(i, a, b):
    mid1 = i.some.per(.5, a[0], a[1])
    mid2 = i.some.per(.5, b[0], b[1])
    if abs(mid1 - mid2) < i.tiny:  # gap too small
      return True
    sa = i.some.sd(a[0], a[1])
    sb = i.some.sd(b[0], b[1])
    sboth = i.some.sd(a[0], b[1])
    if (sboth < sa and sboth < sb):  # smaller gaps more confusing
      return True


class Row(o):
  def __init__(i, tab, cells):
    i._tab = tab
    i.cells = cells
    i.ranges = cells[:]


class Tab(o):
  num = "$"
  less = "<"
  more = ">"
  skip = "?"
  nums = "><$"
  goal = "<>!"
  klass = "!"

  def __init__(i, lst):
    i.cols, i.rows, i.x, i.y, i.syms, i.nums = [], [], [], [], [], []
    [i.row(row) if i.cols else i.header(row) for row in lst]
    i.discretize()
    print(i.rows[0])

  def header(i, lst):
    for pos, txt in enumerate(lst):
      one = Some(pos, txt)
      i.cols.append(one)
      (i.nums if txt[0] in Tab.nums else i.syms).append(one)
      (i.x if txt[0] in Tab.goal else i.y).append(one)

  def row(i, lst):
    row = Row(i, [col.add(x) for col, x in zip(i.cols, lst)])
    i.rows.append(row)

  def discretize(i):
    for n in i.nums:
      n.bins = Bins(n).all
      for row in i.rows:
        row.ranges[n.pos] = num2range(row.cells[n.pos], n.bins)


def num2range(x, a):
  if x == Tab.skip:
    return x
  n = len(a)
  for j in range(1, len(a)):
    if a[j - 1] <= x < a[j]:
      return j / len(a)
  return (len(a) - 1) / len(a)


class TestSimp(unittest.TestCase):
  def test_tab(i):
    t = Tab(auto93)
    assert(len(t.cols) == 8)

  def test_some(i):
    seed(1)
    s = Some()
    for j in range(246):
      s.add(j)
    print("p", s.per(.5))
    assert(s.per(.5) == 123)

  def test_bins(i):
    seed(1)
    t = Tab(auto93)
    for x in t.x:
      if x.nump:
        b = Bins(x)
        print("\n" + x.txt, x.all()[0], x.all()[-1])
        print("S", b.splits)
        print("S", b.all)

  def test_ranges1(i):
    def y(x):
      if x < 0.3:
        return 1 + random() / 2
      if x < 0.6:
        return 2 + random() / 2
      return 3 + random() / 2
    seed(1)
    a = sorted([y(random()**2) for _ in range(10000)])
    print("range", a[0], a[-1])
    s = Some(inits=a)
    print("bins", Bins(s).all)

  def test_ranges(i):
    a = [10, 20, 30, 40]
    print("aa", 5, num2range(5, a))
    print("aa", 25, num2range(25, a))
    print("aa", 60, num2range(60, a))


if __name__ == '__main__':
  unittest.main()
