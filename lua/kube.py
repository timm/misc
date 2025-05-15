#!/usr/bin/env python3 -B
"""
kube.py : barelogic, XAI for active learning + multi-objective optimization
(c) 2025, Tim Menzies <timm@ieee.org>, MIT License  

Options:  

      -b bins    number of bins                     = 5
      -m min     minPts per cluster (0=auto choose) = 0
      -P P       distance formula exponent          = 2  
      -d dims    number of dimensions               = 4
      -r rseed  random number seed                  = 1234567891
      -s some    search space size for poles        = 30
      -f file    training csv file = ../../moot/optimize/misc/auto93.csv  
"""
import random, sys, re
any = random.choice
many= random.choices

BIG = 1E32

class o:
  __init__ = lambda i,**d: i.__dict__.update(**d)
  __repr__ = lambda i: cat(i)

#----------------------------------------------------------------------------------------
class Sym(o):
  def __init__(i, has=[], at=0, txt= " "): 
    i.at, i.txt, i.n = at, txt, 0
    i.has = {}
    [i.add(x) for x in has]
  
  def add(i, x, inc=True):
    if x !="?":
      step = 1 if inc else -1
      i.n += step
      i.has[x] = step + (i.has[x] if x in i.has else 0) 
    return x

  def dist(i,x,y): return x=="?" and y=="?" and 1 or x != y
  def mid(i): return max(i.has, key=i.has.get)
  def div(i): return -sum(v/i.n * math.log(v/i.n,2) for v in i.has.values() if v > 0)
 
#----------------------------------------------------------------------------------------
class Num(o):
  def __init__(i,has=[],at=0,txt=" "):
    i.at, i.txt, i.n = at, txt, 0
    i.mu, i.m2, i.lo, i.hi, i.goal = 0,0,BIG,-BIG, 0 if txt[-1]=="-" else 1
    [i.add(x) for x in has]
  
  def add(i, x, inc=True):
    if x != "?":
      i.lo = min(x, i.lo)
      i.hi = max(x, i.hi)
      if not inc and i.n <= 2:
        i.n = i.mu = i.m2 =  0
      else:
        step = 1 if inc else -1
        i.n += step
        d = x- i.mu
        i.mu += step * d/i.n
        i.m2 += step * d * (x - i.mu)
    return x

  def dist(i,x,y):
    if x == "?" and y == "?": return 1
    x = i.norm(x) if x != "?" else (0 if y > 0.5 else 1)
    y = i.norm(y) if y != "?" else (0 if x > 0.5 else 1)
    return abs(x - y)

  def mid(i): return i.mu 
  def norm(i,x): return  (x - i.lo) / (i.hi - i.lo + 1/BIG)
  def div(i): return 0 if i.n <=2  else (max(0,i.m2)/(i.n-1))**.5

#----------------------------------------------------------------------------------------
class Rows(o):
  def __init__(i,src): 
    i._rows, i.cols = [], o(x=[], y=[], all=[])
    src = iter(src)
    [i.about(c,s) for c,s in enumerate(next(src))]
    [i.add(row) for row in src]

  def about(i,c,s):
    col = (Num if s[0].isupper() else Sym)(at=c,txt=s)
    i.cols.all += [col]
    if s[-1] != "X":
      (i.cols.y if s[-1] in "-+" else i.cols.x).append(col)

  def add(i,row,inc=True,purge=False):
    if purge: i._rows.remove(row) # can be slow. disabled by default
    else: i._rows += [row]
    for col in i.cols.all: col.add(row[col.at], inc)
    return row

  def clone(i, rows=[]): 
    return Rows([[col.txt for col in i.cols.all]] + rows)

  def lsh(i, poles):
    clusters={}
    for row in i._rows:
      k = tuple(i.project(row,a,b) for a,b in zip(poles, poles[1:]))
      clusters[k] = clusters.get(k) or i.clone()
      clusters[k].add(row)
    return clusters

  def mid(i):
    middle = [col.mid() for col in i.all.cols]
    return min(i._rows, key=lambda row: i.xdist(row,middle))

  def poles(i):
    "Select a pole at max distance to poles picked so far."
    r0, *some = many(i._rows, k=the.some+1)
    out = [max(some, key = lambda r1: i.xdist(r1,r0))]
    for _ in range(the.dims):
      out += [max(some, key=lambda r2: sum(i.xdist(r2,r1) for r1 in out))]
    return out

  def project(i,row,a,b):
    c = i.xdist(a,b)
    x = (i.xdist(row,a)**2 + c**2 - i.xdist(row,b)**2) / (2*c)
    return min(int( x/c * the.bins), the.bins - 1)

  def xdist(i,row1,row2):  return dist(c.dist(row1[c.at],row2[c.at])   for c in i.cols.x)
  def ydist(i,row):        return dist(abs(c.norm(row[c.at]) - c.goal) for c in i.cols.y)
  def ydists(i,rows=None): return Num(i.ydist(row) for row in rows or i._rows)

#----------------------------------------------------------------------------------------
def cat(x):
  isa=isinstance
  if isa(x, list): return "{" + ", ".join(map(cat, x)) + "}"
  if isa(x, (float,int)): return str(x) if x ==int(x) else f"{x:.3g}"
  if isa(x, dict): return cat([f":{k} {cat(v)}" for k,v in x.items() if str(k)[0]!="_"])
  if hasattr(x,"__dict__"): return x.__class__.__name__ + cat(x.__dict__)
  return str(x)

def cli(d):
  for k,v in d.items():
    for c,arg in enumerate(sys.argv):
      if arg == "-"+k[0]: 
        d[k] = coerce("False" if str(v) == "True"  else (
                      "True"  if str(v) == "False" else (
                      sys.argv[c+1] if c < len(sys.argv) - 1 else str(v))))
def coerce(x):
  for what in (int, float):
    try: return what(x)
    except: pass
  x = x.strip()
  y = x.lower()
  return (y== 'true') if y in ("true", "false") else x

def csv(path):
  with open(path) as f:
    for line in f:
      yield [coerce(x) for x in line.strip().split(",")]

def dist(dims): 
   n, d = 0, 1/BIG
   for x in dims:
     d  = d + 1
     n += x**the.P
   return (n / d) ** (1 / the.P)

#---------------------------------------------------------------------------------------/
def eg_h(_): print(__doc__)

def eg__all(_):
  for f in [eg__the, eg__csv,eg__data,eg__ydist,eg__poles, eg__counts]:
    random.seed(the.rseed)
    f(_)

def eg__the(_): print(the)

def eg__csv(_): [print(row) for row in csv(the.file)]

def eg__data(_): 
  d = Rows(csv(the.file))
  [print("x",col)  for col in d.cols.x]
  [print("y",col)  for col in d.cols.y]

def eg__ydist(_):
  d = Rows(csv(the.file))
  lst = sorted(d._rows,key=lambda row: d.ydist(row))
  for row in lst[:4] : print("good",row)
  for row in lst[-4:] : print("bad",row)

def eg__poles(file=None):
  d = Rows(csv(file or the.file))
  p = d.poles()
  dims = d.lsh(p)
  [print(k) for k in dims]
  print(len(dims))

def eg__counts(file=None):
  d = Rows(csv(file or the.file))
  print(file or the.file)
  p = d.poles()
  clusters = d.lsh(p)
  m = the.min
  if m == 0:
    if len(d._rows) < 30: m=2
    elif len(d._rows) < 100: m=3
    else: m= 2*the.dims
  for data in clusters.values():
    ys = data.ydists()
    if len(data._rows) >= m:
      print(o(mid=ys.mid(), div=ys.div(),n=ys.n))

#---------------------------------------------------------------------------------------
the = o(**{m[1]:coerce(m[2]) for m in re.finditer(r"-\w+\s*(\w+).*=\s*(\S+)",__doc__)})

if __name__ == "__main__":
  cli(the.__dict__)
  for n,s in enumerate(sys.argv):
    if fun := globals().get("eg" + s.replace("-","_")):
      random.seed(the.rseed)
      fun(None if n==len(sys.argv) - 1 else coerce(sys.argv[n+1]))

