#!/usr/bin/env python3
""""
asdas
"""

import re
import sys
import math
import copy
import bisect
import unittest
from tests import *
from data import auto93
from docopt import docopt
from random import random, seed, choice
from random import shuffle as rshuffle

usage = """Simp

Usage:
    simp.py [options]

Options:
    -h          Help.
    -v          Verbose.
    --seed=<n>  Set random number seed [default: 1].
    -k=<n>      Speed in knots [default: 10].

"""


def opt(d):
  d = o(v="-v" in d,
        seed=int(d["--seed"]),
        k=int(d["-k"]))
  seed(d.seed)
  return d


def same(x): return x
def first(a): return a[0]
def last(a): return a[-1]


def shuffle(a):
  rshuffle(a)
  return a


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

  def get(i, x, default=0):
    return i.__dict__.get(x, default)


class Row(o):
  def __init__(i, tab, cells):
    i._tab = tab
    i.cells = cells
    i.bins = cells[:]
    i.seen = False
    i.dom = 0

  def __getitem__(i, k):
    return i.cells[k]

  def better(i, j):
    c = i._tab.cols
    s1, s2, n = 0, 0, len(c.y) + 0.0001
    for k in c.y:
      x = i.bins[k]
      y = j.bins[k]
      s1 -= math.e**(c.w[k] * (x - y) / n)
      s2 -= math.e**(c.w[k] * (y - x) / n)
    return s1 / n < s2 / n

  def status(i):
    return [i[y] for y in i._tab.cols.y]


class Tab(o):
  ch = o(klass="!", num="$",
         less="<", more=">", skip="?",
         nums="><$", goal="<>!,")

  def __init__(i, lst=[]):
    i.rows = []
    i.cols = o(all={}, w={}, klass=None, x={}, y={}, syms={}, nums={})
    i._bins = {}
    [i.add(row) for row in cols(rows(lst))]

  def add(i, row):
    i.row(row) if i.cols.all else i.header(row)

  def header(i, lst):
    c, ch = i.cols, Tab.ch
    c.klass = -1
    for pos, txt in enumerate(lst):
      c.all[pos] = txt
      (c.nums if txt[0] in ch.nums else c.syms)[pos] = txt
      (c.y if txt[0] in ch.goal else c.x)[pos] = txt
      c.w[pos] = -1 if ch.less in txt else 1
      if ch.klass in txt:
        c.klass = pos

  def row(i, lst):
    i.rows += [Row(i, lst)]

  def bins(i, want=None):
    for x in i.cols.nums:
      i._bins[x] = bins = numbins(
          i.rows, want=want, x=x, y=i.cols.klass)
      for row in i.rows:
        row.bins[x] = place(bins, row[x])


def place(runs, x):
  if x == "?":
    return x
  n = len(runs)
  for pos, run in enumerate(runs):
    #print(x, run.xlo)
    if x < run.xlo:
      break
    if run.xlo <= x < run.xhi:
      break
  return round((pos + 1) / n, 2)


def symbins(lst, x=0, y=-1, want=True, *_):
  def run1(z="__all__"):
    return o(xlo=z, xhi=z, x=x, ys=o(), xmid=z, val=0)
  all = run1()
  bins = {}
  for z in lst:
    xx, yy = z[x], z[y]
    if xx != "?":
      if xx not in bins:
        bins[xx] = run1(xx)
      one = bins[xx]
      klass = 1 if yy == want else 0
      one.ys.inc(klass)
      all.ys.inc(klass)
  return [bore(one, all) for one in bins.values()]


def bore(z, all, e=0.00001):
  yes = z.ys.get(1) / (all.ys.get(1) + e)
  no = z.ys.get(0) / (all.ys.get(0) + e)
  tmp = yes**2 / (yes + no + e)
  z.val = tmp if tmp > 0.01 else 0
  return z


def numbins(lst, x=0, y=-1, want=True,
            cohen=.2, enough=.2, trivial=.05):
  def run1(z="__all__"):
    return o(xlo=z, xhi=z, x=x, ys=o(), xmid=0, val=0)

  def it(n): return lst[min(n, len(lst) - 1)]

  def ok(z, e=0.000000001):
    z.xmid = (it(z.xhi)[x] + it(z.xlo)[x]) / 2
    return bore(z, all)

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
      klass = 1 if z[y] == want else 0
      now.ys.inc(klass)
      all.ys.inc(klass)
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
        if abs(a.xmid - b.xmid) < d or ab.val >= a.val and ab.val >= b.val:
          a = ab
          j += 1
      tmp += [a]
      j += 1
    return tmp if len(tmp) == len(runs) else merge(tmp, 0, [])

  def xplain(z):
    z.xlo = it(z.xlo)[x]
    z.xhi = it(z.xhi)[x]
    return z

  # --------------------------------------------------------------
  lst = sorted((z for z in lst if z[x] != "?"), key=lambda z: z[x])
  all = run1(0)
  tmp = [xplain(run) for run in merge(split())]
  print(all)
  return tmp


def smo(tab, n1=10):
  lst = shuffle(tab.rows)
  for i, j in pairs(lst[:n1]):
    i.dom += i.better(j)


def pairs(lst):
  j = 0
  while j < len(lst) - 1:
    yield lst[j], lst[j + 1]
    j += 2


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


if __name__ == '__main__':
  my = opt(docopt(usage))
  print(my)
  unittest.main()
