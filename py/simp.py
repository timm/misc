import unittest
from random import random, seed
from data import auto93
import collections


def Nested(): return collections.defaultdict(
    lambda: collections.defaultdict(Nested))


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

  def __add__(i, j):
    a = i.__dict__
    b = j.__dict__
    for k in b:
      a[k] = a.get(k, 0) + b[k]


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
        i.update(i._all.append(x))
      elif random() < Some.hi / i.n:
        i.update(i.replace(x))
    return x

  def update(i, ignore):
    i.sorted = False
    i.n += 1

  def replace(i, x):
    i._all[int(len(i._all) * random())] = x

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

  def nmusd(i):
    return len(i._all), i.per(), i.sd()

  def same(i, j):
    xn, xmu, xsd = i.nmusd()
    yn, ymu, ysd = j.nmusd()
    return Ttest(xn, xmu, xsd, yn, ymu, ysd)


class Ttest(o):
  small = 0.38  # medium = 1
  d = {}
  d[95] = [[3, 3.182], [6, 2.447], [12, 2.179],
           [24, 2.064], [48, 2.011], [96, 1.985]]
  d[99] = [[3, 5.841], [6, 3.707], [12, 3.055],
           [24, 2.797], [48, 2.682], [96, 2.625]]

  def __init__(i, *lst, conf=95):
    xy = Ttest.d[conf]
    i.result = i.hedges(Ttest.small, *lst) and i.ttest(xy, *lst)

  def hedges(i, threshold, xn, xmu, xsd, yn, ymu, ysd):
    # from https://goo.gl/w62iIL
    nom = (xn - 1) * xsd ** 2 + (yn - 1) * ysd ** 2
    denom = (xn - 1) + (yn - 1)
    sp = (nom / denom)**0.5
    g = abs(xmu - ymu) / sp
    c = 1 - 3.0 / (4 * (xn + yn - 2) - 1)
    return g * c > threshold

  def ttest(i, xy, xn, xmu, xsd, yn, ymu, ysd, conf=95):
    # debugged using https://goo.gl/CRl1Bz
    t = (xmu - ymu) / max(10 ** -64, xsd**2 / xn + ysd**2 / yn)**0.5
    a = xsd ** 2 / xn
    b = ysd ** 2 / yn
    df = (a + b)**2 / (10 ** -64 + a**2 / (xn - 1) + b**2 / (yn - 1))
    c = i.critical(xy, int(df + 0.5))
    return abs(t) > c

  def critical(i, xy, df):
    x1, y1 = xy[0]
    if df < x1:
      return y1
    for x2, y2 in xy[1:]:
      if x1 <= df < x2:
        return y1 + (y2 - y1) * (df - x1) / (x2 - x1)
      x1, y1 = x2, y2
    return y2


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
      n.bins = Unsuper(n).bins
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


class TestSimp(o):  # unittest.TestCase):
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
    a = sorted([y(random()**2) for _ in range(10000)])
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


#if __name__ == '__main__': unittest.main()
