from fileinput import input as finput
from types import SimpleNamespace as o

big=1E32

the = o(bins=12, n=23, seed=1234567891,
        file="../../../moot/optimize/misc/auto93.csv")

def csv(files=None):
  for line in finput(files=files):
    if line: yield [s.strip() for s in line.split(",")]

def data(src):
  src = iter(src)
  i = o(it=data, rows=[], cols=_cols(next(src)))
  [row(i,r) for r in src]
  return i 

def _cols(names):
  all,y = [],{}
  for c,s in enumerate(names):
    all += [(big,-big) if s[0].isupper() else {}]
    if s[-1] in "!-+": y[c] = s[-1]!="-"
  return o(all=all, y=y, names=names)

def row(i, row):
  i.rows += [row]
  for c,col in enumerate(i.cols.all):
      if (v:=row[c]) != "?":
        if type(col) is tuple:
          v = row[c] = float(v)
          lo,hi = i.cols.all[c]
          i.cols.all[c] = (min(lo,v), max(hi,v))

def norm(lo,hi,x): return (x - lo) / (hi - lo + 1/big)

def ydist(i,row):
  d = 0
  for c,w in enumerate(i.cols.y): 
    d += abs(norm(*i.cols.all[c], row[c]) - w)**2
  return (d/len(i.cols.y)) ** 0.5

def ydists(i,rows=None):
  return sorted(rows or i.rows, key=lambda r:ydist(i,r))

d = data(csv())
ydists(d)

