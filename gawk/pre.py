#!/usr/bin/env python3 -B
"""
pre.py (v0.5): lightweight XAI for multi-objective optimization   
(c) 2025, Tim Menzies <timm@ieee.org>, MIT license      

Options:
   
    -b  bins for discretization: bins=5           
    -f  data file              : file=auto93.csv   
    -F  how few samples to use : Few=256
    -p  distance coeffecient   : p=2
    -s  random number seed     : seed=42
    -h  show help   
"""
from math import exp,sqrt
import fileinput, random, sys,re
from types import SimpleNamespace as obj

the = obj(bins=5, seed=42, p=2, Few=256, file="auto93.csv")

BIG = 1e32

# Prudence check: does 'the' match the __doc__?
got  = re.findall(r'(\w+)=(\S+)', __doc__)
want = ((k, str(v)) for k, v in vars(the).items())
assert not (bad := set(want) ^ set(got)), f"Mismatch: {bad}"

#------------------------------------------------------------------------------
### Create 

# Nums and Syms are for summarizing numerics and symbolics.
def Sym(at=0,txt=" "): return obj(it=Sym, n=0, at=at, txt=txt, has={})
def Num(at=0,txt=" "): return obj(it=Num, n=0, at=at, txt=txt, mu=0, m2=0, sd=0,
                                  mins={}, lo=BIG, hi=-BIG, best=txt[-1]!="-")

# Data stores (1) rows as well as (2) summaries of each column.
def Data(src):
  src = iter(src)
  return adds(src, obj(it=Data, n=0, rows={}, cols=Cols(next(src))))

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
def sub(x,v): return add(x, v, inc=-1)

# To add v, jump over 'dont knows', increment summaries, then return v.
# To do the update,  change the rows then recursively update column summaries.
def add(x, v, inc=1):
  if v == "?": return v
  x.n += inc
  if x.it is Data:
    [add(col, v[col.at], inc) for col in x.cols.all]
    if inc > 0: x.rows[id(v)] = v
    else: x.rows.pop(id(v))
  elif x.it is Sym:
    x.has[v] = inc + x.has.get(v,0)
  elif x.it is Num:
    x.lo, x.hi = min(v, x.lo), max(v, x.hi)
    if inc < 0 and x.n < 2:
      x.sd = x.mu = x.m2 = x.n = 0
    else:
      d = v - x.mu
      x.mu += inc * d / x.n
      x.m2 += inc * d * (v - x.mu)
      x.sd = 0 if x.n < 2 else sqrt(max(0, x.m2) / (x.n - 1))
  return v

def dist(src):
  n,d=0,0
  for x in src: n,d = n+1,d+x**the.p
  return (d/n) ** (1/the.p)

def distx(data,row1,row2):
  def fn(col,a,b):
    if a==b=="?": return 1
    if col.it is Sym: return a != b
    a,b = norm(col,a), norm(col,b)
    a = a if a!="?" else (0 if b>.5 else 1)
    b = b if b!="?" else (0 if a>.5 else 1)
    return abs(a-b)
  return dist(fn(col,row1[col.at],row2[col.at]) for col in data.cols.x)

def norm(col,x):
  return x if x=="?" or col.it is Sym else (x-col.lo)/(col.hi-col.lo+1/BIG)

def poles(data,rows):
  D = lambda r1,r2: distx(data, r1, r2)
  x,*rest = random.choices(rows, few=the.Few)
  y = max(rest, key=lambda r: D(r,x))
  z = max(rest, key=lambda r: D(r,y))
  c = D(y,z)
  P = lambda r: (D(r,y)**2 + c**2 - D(r,y)**2)/ (2 * c) <= c/2
  return {id(row):P(row) for row in rows}

#------------------------------------------------------------------------------
### Discretize

# Return the smallest value ever seen in v's bin.
# Note that to find the true min, this has to be run once over all the data,
# then a second time to return the actual min bin value.
def bin(col, v):
  if v == "?" or col.it is not Num: return v
  z = (v - col.mu) / (col.sd + 1/BIG)
  b = min(the.bins - 1, max(0, int(the.bins / (1 + exp(-z)))))
  col.mins[b] = min(v, col.mins.get(b, BIG))
  return col.mins[b]

#------------------------------------------------------------------------------
### Lib

# Pretty print.
def oo(x): print(o(x)); return x

def o(x, d=1):
  if   (d:=d+1) > 8     : x= "..."
  elif callable(x)      : x= x.__name__
  elif type(x) is obj   : x= o(vars(x),d)
  elif type(x) is list  : x= ', '.join([o(y,d) for y in x])
  elif type(x) is float : x= int(x) if x % 1 == 0 else round(x,3)
  elif type(x) is dict  : x= "{"+" ".join(f":{k} {o(x[k],d)}" for k in x)+"}"
  return str(x)

# Read csv files.
def csv(file=None):
  for line in fileinput.input(files=file if file else '-'):
    if (line := line.split("%")[0]):
      yield [coerce(s.strip()) for s in line.split(",")]

def coerce(s):
  try: return int(s)
  except:
    try: return float(s)
    except: return {'True':True, 'False':False}.get(s,s)

#------------------------------------------------------------------------------
## Demos

def eg_h(): print(__doc__)

def eg__bin():
  data = Data(csv(the.file))
  [bin(col, row[col.at]) for row in data.rows.values() for col in data.cols.x]
  oo(data.cols.names)
  for row in data.rows.values():
    tmp=row[:]
    for col in data.cols.x: tmp[col.at] = bin(col, tmp[col.at])
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
