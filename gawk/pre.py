#!/usr/bin/env python3 -B
from types import SimpleNamespace as obj
from pathlib import Path
from math import exp

BIG=1e32

the=obj(bins=5,
        file=Path.home() / "gits/timm/moot/optimize/misc/auto93.csv")

#------------------------------------------------------------------------------
def Sym(i=0,s=" "): return obj(it=Sym, i=i, s=s, n=0, has={})
def Num(i=0,s=" "): return obj(it=Num, i=i, s=s, n=0, mu=0, _m2=0, sd=0,mins={},
                              lo=BIG, hi=-BIG, best=0 if s[-1]=="-" else 1)

def Data(src): 
  src = iter(src)
  return adds(src, obj(it=Data, n=0, rows={}, cols=Cols(next(src))))

def Cols(names):
  all = [(Num if s[0].isupper() else Sym)(i,s) for i,s in enumerate(names)]
  return obj(it=Cols, names=names, all=all, 
             y = [col for col in all if col.s[-1]     in "-+"], 
             x = [col for col in all if col.s[-1] not in "-+X"])

def clone(data,rows=[]): return Data([data.cols.names] + rows)

#------------------------------------------------------------------------------
def adds(lst,it=None):
  it = it or Num()
  [add(it,x) for x in lst]
  return it

def add(x,v):
  if v!="?": 
    x.n += 1
    if x.it is Sym:  
      x.has[v] = 1 + x.has.get(v,0)
    elif x.it is Num:
      d      = v-x.mu
      x.mu  += d / x.n
      x._m2 += d * (v - x.mu) 
      x.sd   = 0 if x.n < 2 else (max(0,x._m2) / (x.n - 1)) ** .5
      x.lo   = min(v, x.lo)
      x.hi   = max(v, x.hi)
    else:
      [add(col,v[col.i]) for col in x.cols.all]
      x.rows[id(v)] = v
  return v

def bin(col, v):
  return v if v == "?" or col.it is not Num else col.mins[_bin(col, v)]

def _bin(col, v):
  if v == "?" or col.it is not Num: return v
  z = (v - col.mu) / (col.sd + 1/BIG)
  b = min(the.bins - 1, max(0, int(the.bins / (1 + exp(-z)))))
  col.mins[b] = min(v, col.mins.get(b, BIG))
  return b

#------------------------------------------------------------------------------
def csv(file):
  with open(file,encoding="utf-8") as f:
    for line in f:
      if (line := line.split("%")[0]):
        yield [coerce(s.strip()) for s in line.split(",")]

def coerce(s):
  try: return int(s)
  except:
    try: return float(s)
    except: return {'True':True, 'False':False}.get(s,s)

def oo(x): print(o(x)); return x
def o(x):
  if   callable(x)      : x= x.__name__
  elif type(x) is obj   : x= o(vars(x))
  elif type(x) is list  : x= ', '.join([o(x1) for x1 in x])
  elif type(x) is float : x= str(x//1) if x % 1 == 0 else f"{x:.3f}"
  elif type(x) is dict  : x= "{"+" ".join(f":{k} {o(x[k])}" for k in x)+"}"
  return str(x)

#------------------------------------------------------------------------------
data = Data(csv(the.file))
[_bin(col, row[col.i]) for row in data.rows.values() for col in data.cols.x]
oo(data.cols.names)
for row in data.rows.values():
  tmp=row[:]
  for col in data.cols.x: tmp[col.i] = bin(col, tmp[col.i])
  oo(tmp)
