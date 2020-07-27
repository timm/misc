import unittest
from random import random, seed
from data import auto93
import copy
from docopt import docopt as cli
from collections import namedtuple

usage = """Simp

Usage: simp.py [options]

 -h  help
 -k [0.9]

"""


def same(x): return x
def first(a): return a[0]
def last(a): return a[-1]


def Nested(): return collections.defaultdict(
    lambda: collections.defaultdict(Nested))


class o:
  "Class that can pretty print."
  def __init__(i, **d):
    i.__dict__.update(**d)

  def __repr__(i):
    d, n = i.__dict__, i.__class__.__name__
    return n + "{" + ', '.join(
        [(':%s %s' % (k, d[k])) for k in sorted(d.keys())
            if str(k)[0] != "_"]) + "}"

  def inc(i, x):
    i.__dict__[x] = i.__dict__.get(x, 0) + 1

  def add(i, j):
    k = copy.deepcopy(i)
    for x in j:
      k.__dict__[x] = j[k] + k.__dict__.get(x, 0)
    return k


class Sorted(o):
  def __init__(i, a, x=0, y=-1):
    i.x, i.y, i.has = x, y, sorted(
        (z for z in a if z[x] != "?"), key=lambda z: z[x])

  def per(i, p=.5, lo=0, hi=None):
    hi = hi or len(i.has) - 1
    j = lo + (hi - lo) * p
    return i.has[int(j)][i.x]

  def sd(i, lo=None, hi=None):
    return (i.per(.9, lo, hi) - i.per(.1, lo, hi)) / 2.54


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
    i.rows = []
    i.cols = o(all={}, klass=None, x={}, y={}, syms={}, nums={})
    [i.row(row) if i.cols.all else i.header(row) for row in lst]

  def header(i, lst):
    c = i.cols
    for pos, txt in enumerate(lst):
      c.all[pos] = txt
      (c.nums if txt[0] in Tab.nums else c.syms)[pos] = txt
      (c.x if txt[0] in Tab.goal else c.y)[pos] = txt
      if Tab.klass in txt:
        c.klass = pos

  def row(i, lst):
    i.rows += [Row(i, cells)]

  def discretize(i):
    for j in i.cols.nums:
      s = Sorted(rows, x=j, y=i.cols.klass)
      b = Bins(s, col=j)


 def bins(s,   
         col=j,
         cohen=0.3,  # trivial difference = cohen*sd
         enough=0.5, # min size of each bin is size**enough
         want=None):
   want=want
   enough=len(a)**enough
   tiny=cohen * s.ad()
   all=dict()
   n1, lst=0, [[0, 0, o(), col, all]]
   for n2, xy in enumerate(a):
     if n2 - n1 >= i.enough:  # enough here
       if len(a) - n2 >= i.enough:  # enough after
         if fx(a[n2]) != fx(a[n2 - 1]):  # we can split here
           lst += [[n2, n2, o(), col, all]]
           n1=n2
     lst[-1][1]=n2 + 1
     if i.want:
        lst[-1][2].inc(fy(a[n2]) == i.want)



   splits=split(a)
   i.bins=[a[x] for x, _ in
               i.merge(i.split(a, col, fx, fy), some)]

   def split(i, a, col, fx, fy):
        return lst

   def merge1(a, b):
     return [a[0], b[1], a[2].add(b[2]), a[3], a[4]]

   def merge(i, b4, some):
     j, after=0, []
     while j < len(b4):
       a=b4[j]
       if j < len(b4) - 1:
         b=b4[j + 1]
         c=a.add(b)
         if i.bothIsBetter(a, b, some):
           a=[a[0], b[1]]
           j += 1
       after += [a]
       j += 1
     return i.merge(after, some) if len(after) < len(b4) else after

   def bothIsBetter(i, a, b, some):
     mid1=some.per(.5, a[0], a[1])
     mid2=some.per(.5, b[0], b[1])
     if abs(mid1 - mid2) < i.tiny:  # gap too small
       return True
     sa=some.sd(a[0], a[1])
     sb=some.sd(b[0], b[1])
     sboth=some.sd(a[0], b[1])
     if (sboth < sa and sboth < sb):  # smaller gaps more confusing
       return True


class Unsuper:
  cohen = 0.3  # trivial difference = cohen*sd
  enough = 0.5  # min size of each bin is size**enough

  def __init__(i, some, want=None):
    a = some.all()
    i.want = want
    i.enough = len(a)**Unsuper.enough
    i.tiny = Unsuper.cohen * some.sd()
    splits = i.split(a)
    i.bins = [a[x] for x, _ in i.merge(i.split(a), some)]

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

  def merge(i, b4, some):
    j, after = 0, []
    while j < len(b4):
      a = b4[j]
      if j < len(b4) - 1:
        b = b4[j + 1]
        if i.bothIsBetter(a, b, some):
          a = [a[0], b[1]]
          j += 1
      after += [a]
      j += 1
    return i.merge(after, some) if len(after) < len(b4) else after

  def bothIsBetter(i, a, b, some):
    mid1 = some.per(.5, a[0], a[1])
    mid2 = some.per(.5, b[0], b[1])
    if abs(mid1 - mid2) < i.tiny:  # gap too small
      return True
    sa = some.sd(a[0], a[1])
    sb = some.sd(b[0], b[1])
    sboth = some.sd(a[0], b[1])
    if (sboth < sa and sboth < sb):  # smaller gaps more confusing
      return True


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
        b = Unsuper(x)
        print("\n" + x.txt, x.all()[0], x.all()[-1])
        print("S", b.bins)

  def test_ranges1(i):
    def y(x):
      if x < 0.3:
        return 1 + random() / 2
      if x < 0.6:
        return 2 + random() / 2
      return 3 + random() / 2
    seed(1)
    a = sorted([y(random() ** 0.5) for _ in range(10000)])
    print("range", a[0], a[-1])
    s = Some(inits=a)
    print("bins", Unsuper(s).bins)

  def test_ranges(i):
    a = [10, 20, 30, 40]
    print("aa", 5, num2range(5, a))
    print("aa", 25, num2range(25, a))
    print("aa", 60, num2range(60, a))

  def test_rtest(i):
    a = [random() for _ in range(1000)]
    n = 1
    while n < 1.7:
      n += .05
      b = [n * x for x in a]
      sa = Some(inits=a)
      sb = Some(inits=b)
      print("!!!", f"{n: 0.2f}", sa.same(sb))


if __name__ == '__main__':
  print(: w
        cli(usage))  # unittest.main():1
  : 1
