import re
import sys
import copy
import bisect
import unittest
from data import auto93
from docopt import docopt
from random import random, seed
from collections import namedtuple

usage = """Simp

Usage:
  simp.py [options]

Options:
  -h          Help.
  -v          Verbose.
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

  def __add__(i, j):
    k = copy.deepcopy(i)
    for x in j.__dict__:
      k.__dict__[x] = j.__dict__[x] + k.__dict__.get(x, 0)
    return k

  def get(i, x, default):
    return i.__dict__.get(x, default)


class Row(o):
  def __init__(i, tab, cells):
    i._tab = tab
    i.cells = cells
    i.bins = cells[:]

  def __getitem__(i, k):
    return i.cells[k]


class Tab(o):
  ch = o(klass="!", num="$",
         less="<", more=">", skip="?",
         nums="><$", goal="<>!,")

  def __init__(i, lst=[]):
    i.rows = []
    i.cols = o(all={}, w={}, klass=None, x={}, y={}, syms={}, nums={})
    [i.add(row) for row in cols(rows(lst))]

  def add(i, row):
    i.row(row) if i.cols.all else i.header(row)

  def header(i, lst):
    c, ch = i.cols, Tab.ch
    for pos, txt in enumerate(lst):
      c.all[pos] = txt
      (c.nums if txt[0] in ch.nums else c.syms)[pos] = txt
      (c.x if txt[0] in ch.goal else c.y)[pos] = txt
      c.w[pos] = -1 if ch.less in txt else 1
      if ch.klass in txt:
        c.klass = pos

  def row(i, lst):
    i.rows += [Row(i, lst)]

  def bins(i):
    for x in i.cols.nums:
      print("")
      for z in bins(i.rows, want=400, x=x, y=i.cols.klass):
        print(z.xlo, z.xhi)


def bins(lst, x=0, y=-1, want=True,
         cohen=.3, enough=.5, epsilon=.05):
  def run1(z):
    return o(xlo=z, xhi=z, x=x, ys=o(), xmid=0, val=0)

  def it(n): return lst[min(n, len(lst) - 1)]

  def ok(z, e=10**-32):
    b = z.ys.get(1, 0) / (e + all.get(1, 0))
    r = z.ys.get(0, 0) / (e + all.get(0, 0))
    z.val = b**2 / (e + b + r) if b > r + epsilon else 0
    z.xmid = (it(z.xhi)[x] + it(z.xlo)[x]) / 2
    return z

  def split(xlo=0, runs=[run1(0)]):
    n = len(lst)**enough
    while n < 10 and n < len(lst) / 2:
      n *= 1.333
    for xhi, z in enumerate(lst):
      if xhi - xlo >= n:
        if len(lst) - xhi >= n:
          if z[x] != lst[xhi - 1][x]:
            runs += [run1(xhi)]
            xlo = xhi
      now = runs[-1]
      now.xhi = xhi + 1
      all.xhi = xhi + 1
      now.ys.inc(z[y] == want)
      all.ys.inc(z[y] == want)
      #print(xhi, y, z[y], want, z[y] == want)
    return [ok(run) for run in runs]

  def merge(runs, j=0, tmp=[]):
    def add(z1, z2): return ok(
        o(xlo=z1.xlo, xhi=z2.xhi, x=x, ys=z1.ys + z2.ys,
          xmid=0, val=0))

    def per(z): return lst[int(len(lst) * z)][x]
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
  all = run1(0)
  return merge(split())


def rows(x=None, f=sys.stdin):
  "Read from stdio or file or string or list. Kill whitespace or comments."
  def items(z):
    for y in z:
      yield y

  def strings(z):
    for y in z.splitlines():
      yield y

  def csv(z):
    with open(z) as fp:
      for y in fp:
        yield y

  if x:
    if isinstance(x, (list, tuple)):
      f = items
    elif x[-3:] == 'csv':
      f = csv
    else:
      f = strings
  for y in f(x):
    if isinstance(y, str):
      y = re.sub(r'([\n\t\r ]|#.*)', '', y).strip()
      if y:
        yield y.split()
    else:
      yield y


def cols(src):
  "Ignore columns if, on line one, the name contains '?'."
  todo = None
  for a in src:
    todo = todo or [n for n, s in enumerate(a) if "?" not in s]
    yield [a[n] for n in todo]


class TestSimp(unittest.TestCase):
  def test_tab(i):
    t = Tab(auto93)
    assert(len(t.cols.all) == 8)
    print(len(t.rows))
    print(t.rows[-1].cells)

  def test_bins(i):
    seed(1)
    t = Tab(auto93)
    t.bins()

  def rest_ranges1(i):
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


def opt(d):
  def val(z):
    if isinstance(z, bool):
      return z
    try:
      return int(z)
    except:
      try:
        return float(z)
      except:
        return z
  return o(**{re.sub(r"^[-]+", "", k): val(d[k]) for k in d})


if __name__ == '__main__':
  my = opt(docopt(usage))
  unittest.main()
