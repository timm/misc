#!/usr/bin/env python3 -B
"""
fft.py, multi objective tree building
(c) 2025, Tim Menzies <timm@ieee.org>, MIT license

Options:
 -s random seed    seed=1234567891
 -d depth of tree  depth=4
 -f data file      file=../moot/regression/auto93.csv
"""
from types import SimpleNamespace as o
import random, math, sys, re

# ---------------------------------------------------------------------
BIG = math.inf

def coerce(z):
  try: return int(z)
  except:
    try: return float(z)
    except:
      z = z.strip()
      return {'True':True, 'False':False}.get(z,z)

the= o(**{k:coerce(v) for k,v in re.findall(r"(\w+)=(\S+)", __doc__)})

# ---------------------------------------------------------------------
def distPoles(data):
  out, east = [], random.choice(data.rows)
  for _ in range(the.depth):
    west = random.choice(data.rows)
    out += [(east, west, distx(data,east,west))]
    east = west
  return out

def disty(data,row):
  d = sum(abs(norm(c,row[c.at]) - c.goal)**the.p for c in data.cols.y)
  return (d / len(data.cols.y))**(1/the.p)

def distx(data,r1,r2):
  def _dist(col):
    a = r1[col.at]
    b = r2[col.at]
    if a==b=="?": return 1
    if isSym(col): return a != b
    a,b = norm(col,a), norm(col,b)
    a = a if a != "?" else (0 if b > .5 else 1)
    b = b if b != "?" else (0 if a > .5 else 1)
    return abs(a - b)
  d = sum(_dist(col)**the.p for col in data.cols.x)
  return (d / len(data.cols.x))**(1/the.p)

def distInterpolate(data,row,east,west,c):
  a = distx(data,row,east)
  b = distx(data,row,west)
  x = (a*a + c*c - b*b) / (2*c + 1/BIG) / (c + 1/BIG)
  y1,y2 = disty(data,east), disty(data,west)
  return y1 + x*(y2 - y1)

def distGuessY(data,row,poles):
  return sum(distInterpolate(data,row,*p) for p in poles)/len(poles)

# ---------------------------------------------------------------------
def Tree(data, Guess, depth=the.depth):
  def _go(data1, d):  
    sub = False
    if d <= depth:
      cuts = [cut for col in data1.cols.x for cut in treeCuts(data1, col, data1.rows, Guess)]
      if cuts:
        best, *_, worst = sorted(cuts)
        for how, (_, c, (xlo, xhi), leaf) in enumerate([worst, best]):
          yes, no = treeKids(data1.rows, c, xlo, xhi)
          if len(yes) > len(data.rows)**.33:
            for subtree in _go(dataClone(data1, no), d + 1):
              sub = True
              yield o(c=c, lo=xlo, hi=xhi, left=leaf, bias=how, right=subtree)
    if not sub:
      yield adds([Guess(row) for row in data1.rows])
  yield from _go(data, 1)

def treeCuts(data, col, rows, Guess):
  ys = {}
  for row in rows:
    x, y = row[col.at], Guess(row)
    if x == "?": continue
    k = x if isSym(col) else x <= col.mu  
    if k not in ys: ys[k] = Num()
    add(ys[k], y)
  return [(ys[k].mu, col.at,
           (k, k) if isSym(col) else ((-BIG, col.mu) if k else (col.mu, BIG)),
           ys[k]) for k in ys]

def treeKids(rows, c, xlo, xhi):
  yes, no, maybe = [], [], []
  for row in rows:
    v = row[c]
    (maybe if v == "?" else yes if xlo <= v <= xhi else no).append(row)
  (yes if len(yes) > len(no) else no).extend(maybe)
  return yes, no

def treeShow(data, t, last=1):
  if not hasattr(t, "c"): print(f"{1-last} : {t.n:>4} : {t.mu:.2f}")
  else:
    name = data.cols.all[t.c].txt
    if   t.lo == t.hi:      txt = f"{name} == {t.hi}"
    elif abs(t.hi) == BIG:  txt = f"{name} >= {t.lo:.3f}"
    else:                   txt = f"{name} <= {t.hi:.3f}"
    print(f"{t.bias} : {t.left.n:>4} : if {txt} then {t.left.mu:.3f} else")
    treeShow(data, t.right, t.bias)

def treePredict(t, row):
  while hasattr(t, "c"):
    t = t.left if row[t.c] == "?" or t.lo <= row[t.c] <= t.hi else t.right
  return t.mu

def treeTune(trees, rows, Guess):
  def _score(t): return sum(abs(Guess(row) - treePredict(t, row)) for row in rows) / len(rows)
  return min(trees, key=_score)

# ---------------------------------------------------------------------
if __name__ == "__main__":

  for k in vars(the):                                 # -x val  overrides the.x
    if ("-" + k[0]) in sys.argv:
      the.__dict__[k] = coerce(sys.argv[sys.argv.index("-" + k[0]) + 1])

  random.seed(the.seed)
  data  = Data(csv(the.file))
  Guess = lambda row: disty(data, row)                # train tree on dist-to-heaven
  trees = list(Tree(data, Guess))
  best  = treeTune(trees, data.rows, Guess)
  treeShow(data, best)
