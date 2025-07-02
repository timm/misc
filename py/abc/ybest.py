from fileinput import input as finput
from types import SimpleNamespace as o
from random import choices as some
import random, sys

big=1E32

the = o(bins=12, n=23, seed=1234567891,
        file="../../../moot/optimize/misc/auto93.csv")

def csv(files=None):
  for line in finput(files=files):
    if line: yield [s.strip() for s in line.split(",")]

def data(src):
  def _cols(names):
    i = o(names=names, all=[], y={})
    for c,s in enumerate(names):
      i.all += [o(lo=big, hi=-big) if s[0].isupper() else {}]
      if s[-1] in "!-+": i.y[c] = s[-1]!="-"
    return i

  src = iter(src)
  i = o(rows=[], cols= _cols(next(src)))
  [add(i,r) for r in src]
  return i 

def add(i, row):
  def _add(col,v,_): 
    if type(col) is dict: 
      col[v] = col.get(v,0) + 1
    else:
      v = row[c] = float(v)
      col.lo = min(v,col.lo)
      col.hi = max(v,col.hi)

  i.rows += [row]
  for c,col in enumerate(i.cols.all):
    if (v:=row[c]) != "?": 
      _add(col,v,c)

def norm(col,x): return (x - col.lo) / (col.hi - col.lo + 1/big)

def xdist(i,row):
  d = 0
  for c,col in enumerate(i.cols.all):
    d += abs(norm(i.cols.all[c], row[c]) - w)**2
  return (d/len(i.cols.y)) ** 0.5

def ydist(i,row):
  d = 0
  for c,w in i.cols.y.items(): 
    d += abs(norm(i.cols.all[c], row[c]) - w)**2
  return (d/len(i.cols.y)) ** 0.5

def guess(d):
  Y = lambda r: ydist(d,r)
  return round(Y(sorted( some(d.rows,k=32), key=Y)[0]),2)

def cli(d):
  for n,arg in enumerate(sys.argv):
    for k in d:
      if arg == "-" + k[0]: 
        d[k] = type(d[k])(sys.argv[n+1])

cli(the.__dict__)
random.seed(the.seed)
d= data(csv(the.file))
print(sorted(guess(d) for _ in range(the.n)))
