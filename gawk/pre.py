#!/usr/bin/env python3 -B
"""
pre.py (v0.5): lightweight XAI for multi-objective optimization   
(c) 2025, Tim Menzies <timm@ieee.org>, MIT license      

Options:
   
    -b  bins=5      bins for discretization         
    -s  seed=42     random number seed 
    -f  file=../moot/optimize/misc/auto93.csv 
    -h  show help   
"""
from types import SimpleNamespace as obj
from pathlib import Path
from math import exp,sqrt
import fileinput, random, sys

BIG=1e32

the=obj(bins=5, seed=42,
        file=Path.home() / "gits/timm/moot/optimize/misc/auto93.csv")

#------------------------------------------------------------------------------
### Create 

# Nums and Syms are for summarizing numerics and symbolics.
def Col(it, at=0, txt=" "): return dict(it=it, at=at, txt=txt, n=0)
def Sym(at=0,txt=" "): return obj(**Col(Sym,at,txt), has={})
def Num(at=0,txt=" "): return obj(**Col(Num,at,txt), mu=0, m2=0, sd=0, 
                                  mins={}, lo=BIG, hi=-BIG, best=txt[-1]!="-")

# Data stores (1) rows as well as (2) summaries of each column.
def Data(src): 
  src = iter(src)
  return adds(src, obj(it=Data, n=0, mid=None, rows={}, cols=Cols(next(src))))

# Columns (which are Nums and Syms) are built from a list of names.
def Cols(names):
  all = [(Num if s[0].isupper() else Sym)(i,s) for i,s in enumerate(names)]
  return obj(it=Cols, names=names, all=all, 
             y = [col for col in all if col.txt[-1]     in "-+"], 
             x = [col for col in all if col.txt[-1] not in "-+X"])

# Copy the structure of a data, maybe add in some rows.
def clone(data,rows=[]): 
  return Data([data.cols.names] + rows)

#------------------------------------------------------------------------------
### Update 

def adds(lst, it=None):
  it = it or Num()
  [add(it,x) for x in lst]
  return it

# To sub, add a negative amount.
def sub(x,v): return add(x, v, -1)

# To add v, jump over 'dont knows', increment summaries, return v.
def add(x, v, inc=1):
  if v!="?": x.n += inc; update(x, v, inc)
  return v

# To update a data, change rows then recursively update column summaries.
# If the increment `inc` is negative, that means remove a row.
def update(x, v, inc):
  if x.it is Data:
    [add(col, v[col.at], inc) for col in x.cols.all]
    if inc > 0 : x.rows[id(v)] = v
    else : x.rows.pop(id(v))
    x.mid = None
  elif x.it is Sym: 
    x.has[v] = inc + x.has.get(v,0)
  else:
    x.lo, x.hi = min(v, x.lo), max(v, x.hi)
    if inc < 0 and x.n < 2: 
      x.sd = x.mu = x.m2 = x.n = 0
    else:
      d     = v - x.mu
      x.mu += inc * d / x.n
      x.m2 += inc * d * (v - x.mu) 
      x.sd  = 0 if x.n < 2 else sqrt(max(0, x.m2) / (x.n - 1))

#------------------------------------------------------------------------------
### Discretize

# Return the smallest value ever seen in v's bin.
def discretize(col, v):
  return v if v == "?" or col.it is not Num else col.mins[bin(col, v)]

# Return v's bin. For Nums, remember the smallest v seen in each bin.
def bin(col, v):
  if v == "?" or col.it is not Num: return v
  z = (v - col.mu) / (col.sd + 1/BIG)
  b = min(the.bins - 1, max(0, int(the.bins / (1 + exp(-z)))))
  col.mins[b] = min(v, col.mins.get(b, BIG))
  return b

#------------------------------------------------------------------------------
### Lib

def csv(file=None):
  for line in fileinput.input(files=file if file else '-'):
    if (line := line.split("%")[0]):
      yield [coerce(s.strip()) for s in line.split(",")]

def coerce(s):
  try: return int(s)
  except:
    try: return float(s)
    except: return {'True':True, 'False':False}.get(s,s)

def oo(x): print(o(x)); return x

def o(x, d=1):
  if   (d:=d+1) > 8     : x= "..."
  elif callable(x)      : x= x.__name__
  elif type(x) is obj   : x= o(vars(x),d)
  elif type(x) is list  : x= ', '.join([o(y,d) for y in x])
  elif type(x) is float : x= int(x) if x % 1 == 0 else round(x,3)
  elif type(x) is dict  : x= "{"+" ".join(f":{k} {o(x[k],d)}" for k in x)+"}"
  return str(x)

#------------------------------------------------------------------------------
## Demos

def eg_h(): print(__doc__)

def eg__bin():
  data = Data(csv(the.file))
  [bin(col, row[col.at]) for row in data.rows.values() for col in data.cols.x]
  oo(data.cols.names)
  for row in data.rows.values():
    tmp=row[:]
    for col in data.cols.x: tmp[col.at] = discretize(col, tmp[col.at])
    print(" ")
    oo(row)
    oo(tmp)

#------------------------------------------------------------------------------
## Start-up
     
if __name__=="__main__" and len(sys.argv) > 1:
  for n,s in enumerate(sys.argv):
    if (fn := globals().get(f"eg{s.replace('-', '_')}")):
      random.seed(the.seed)
      fn()
    else:
      for key in vars(the):
        if s=="-"+key[0]:
          the.__dict__[key] = coerce(sys.argv[n+1])
