import unittest
import re
from random import random, seed
from data import auto93
import copy
from docopt import docopt
from collections import namedtuple

usage = """Simp

Usage: simp.py [options]

Options:
  -h          Help.
  --seed=<n>  Set random number seed [default: 1].
  -k=<n>      Speed in knots [default: 10].
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


Run = namedtuple('Run', 'xlo xhi x xmid ys val')


def runs(lst, xx=0, yy=-1, want=True, cohen=.3, enough=.5, epsilon=.05):
  def run1(z):
    return Run(z, z, xx, o(), 0, 0)

  def ok(z, e=10**-32):
    b = z.ys.get(1, 0) / (e + all.get(1, 0))
    r = z.ys.get(0, 0) / (e + all.get(0, 0))
    z.val = b**2 / (e + b + r) if b > r + epsilon else 0
    z.xmid = (lst[z.xhi] + lst[z.xlo]) / 2
    return z

  def split(xlo=0, runs=[run1(0)]):
    n = len(lst)**enough
    while n < 10 and n < len(lst) / 2:
      n *= 1.333
    for xhi, z in enumerate(lst):
      if xhi - xlo >= n:
        if len(lst) - xhi >= n:
          if z[xx] != lst[xhi - 1][xx]:
            runs += [run1(xhi)]
            xlo = xhi
      now = runs[-1]
      now.xhi = xhi + 1
      all.xhi = xhi + 1
      now.ys.inc(z[yy] == want)
      all.ys.inc(z[yy] == want)
    return [ok(run) for run in runs]

  def merge(runs, j=0, tmp=[]):
    def add(z1, z2): return ok(
        Run(z1.xlo, z2.xhi, xx, 0, z1.ys + z2.ys, 0))

    def per(z): return lst[int(len(lst) * z)][xx]
    d = cohen * (per(.9) - per(.1)) / 2.54
    while j < len(runs):
      a = runs[j]
      if j < len(runs) - 1:
        b = runs[j + 1]
        ab = add(a, b)
        if abs(a.xmid - b.xmid) < d or ab.val > a.val and ab.val > b.val:
          a = ab
          j += 1
      tmp += [a]
      j += 1
    return tmp if len(tmp) == len(runs) else merge(tmp, 0, [])
  # --------------------------------------------------------------
  lst = sorted((z for z in lst if z[x] != "?"), key=lambda z: z[x])
  all = run1[0]
  return merge(split())


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


def atom(z):
  try:
    return int(z)
  except:
    try:
      return float(z)
    except:
      return z


def com(s):
  return o(**{re.sub(r"^[-]+", "", k): atom(v) for k, v in docopt(s).items()})


if __name__ == '__main__':
  print(com(usage).seed)
  # unittest.main():1
