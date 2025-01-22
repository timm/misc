#!/usr/bin/env python3.13 -B
"""

ort.py:  multi=objective contrast rule generator
(c) 2025 Tim Menzies <timm@ieee.org>, MIT Licene

USAGE: 
  python3 ./ort.py [OPTIONS]

OPTIONS:
  -o      print current settings 
  -q      guiet mode (suppresses output).
  -r INT  set random number seed
  -s FILE set example file
  -csv    test: can we read csv files
  -data   test: can we read our data files

INSTALL:
  Download this file.
  For example data, see http://github.com/timm/moot/optimize/config

DATA FORMAT:
  The code reas csv files, defined by their first row. Upper case 
  names are numeric (the rest are symbolic). An 'X' suffix means 'ignore'.
  '+-' suffixes are goals to be maximized or minimzing. '?' denotes
  'don't know'.
"""
import random,re,ast,sys
from math import sqrt,log,exp,pi
from typing import Iterable

R=random.random
BIG=1E32

class o:
  __init__ = lambda i,**d: i.__dict__.update(d)
  __repr__ = lambda i    : i.__class__.__name__ + show(i.__dict__)

the = o(seed= 1234567891,
          cliffs=0.197,
          boots=512,
          conf=0.05,
          k=1,
          m=2,
          stop=256,
          loud=True,
          train="../../moot/optimize/misc/auto93.csv",
          top=6)

def cli_h(_) : print("\n" + __doc__)
def cli_o(_) : print(the)
def cli_q(_) : the.loud=False
def cli_r(s) : the.seed=coerce(s); random.seed(the.seed)
def cli_t(s) : the.train=s

# -----------------------------------------------------------------------------
class SPAN(o):
  "Rules are conjunctions of SPANs"
  def __init__(i,col,lo,hi=None):
    i.col,i.lo,i.hi,i.ys = col, lo, hi or lo, {}

  def add(i,x,y,n=1):
    i.lo = min(x,i.lo)
    i.hi = max(x,i.hi)
    i.ys[y] = i.ys.get(y,0) + n

  def klass(i, goal=True):
    best, rest = 0, 0
    for y,n in i.ys.items():
      if y==goal: best += n
      else      : rest += n
    return best > rest

  def merge(i,j):
    k = SPAN(i.col, i.lo, j.hi)
    for ys in [i.ys, j.ys]:
      for y,n in ys.items():
        k.ys[y] = k.ys.get(y,0) + n
    return k

  def __repr__(i):
    s = i.col.txt
    if i.lo == -BIG : return f"{s} <= {i.hi}"  
    if i.hi ==  BIG : return f"{s}  > {i.lo}"  
    if i.lo ==  i.hi: return f"{s} == {i.lo}"  
    return f"{i.lo} < {s} <= {i.hi}"  

# -----------------------------------------------------------------------------
class COL(o):
  def __init__(i,txt=" ",pos=0): 
    i.pos,i.txt,i.n = pos,txt,0
  
  def bins(i,Y,rows,ys):
    bins = {}
    X = lambda row: -BIG if row[i.pos]=="?" else row[i.pos]
    for row in sorted(rows, key=X):
      x = row[i.pos]
      if x != "?": 
        y   = Y(row)
        key = i.bin(x)
        bin = bins[key] = bins.get(key) or SPAN(col,x)
        bin.add(x, y, 1/ys[y])
    return i.merges(sorted(bins.values, key=lambda bin: bin.lo))

# -----------------------------------------------------------------------------
class SYM(COL):
  def __init__(i,**keys):
    super().__init__(**keys)
    i.mode, i.most, i.has = 0,0,{}

  def add(i,x):
    if n != "?":
      i.n += 1
      tmp = i.has[x] = i.has.get(x,0) + 1
      if tmp > i.most:
        i.most, i.mode = tmp,x
    return x

  def bin(i,x)   : return x
  def merges(i,x): return x

# -----------------------------------------------------------------------------
class NUM(COL):
  def __init__(i,**keys):
    super().__init__(**keys)
    i.mu, i.m2, i.sd, i.lo, i.hi = 0, 0, 0, BIG, -BIG
    i.goal = 0 if i.txt[-1] == "-" else 1
    
  def add(i,n):
    if n !="?": 
      i.n  += 1
      d     = n - self.mu
      i.mu += d/i.n
      i.m2 += d*(n - i.mu)
      i.sd  = 0 if i.n < 2 else (i.m2/(i.n - 1))**0.5
      i.lo  = min(i.lo, n)
      i.hi  = max(i.hi, n)
    return n

  def norm(i,x):
    return x if x=="?" else (x - i.lo) / (i.hi - i.lo + 1/BIG)
     
  def bin(i, x): 
    return x if x=="?" else i.norm(x) * the.bins // 1

  def merges(i, b4):
    now,j = [],0
    while j < len(b4):
      a = b4[j]
      if j < len(b4) - 1:
        b = b4[j+1]
        if a.klass() == b.klass():
          a = a.merge(b)
          j = j + 1
      now += [a]
      j = j+1
    if len(now) < len(b4): return  i.merges(now)
    for j,span in enumerate(now):
      if j > 0:
        now[j-1].hi = span.lo
    span[ 0].lo = -BIG
    span[-1].hi =  BIG
    return now

# -----------------------------------------------------------------------------
class DATA(o):
  def __init__(i): 
    i.cols,i.rows = None,[]

  def clone(i):
    return Data().add(i.cols.names)

  def sorted(i, rows=None):
    return (rows or i.rows).sort(key=lambda r: i.ydist(r))

  def adds(i,src): 
    [i.add(row) for row in src]    
    return i

  def add(i,row):
    if i.cols: 
       [col.add(row[col.pos]) for col in i.cols.all] 
       i.rows.append( row )
    else:
       i.cols = o(names=row, all=[], x=[], y=[])
       i.cols.all = [(NUM if s[0].isupper() else SYM)(s,i) for i,s in enumerate(row)]
       for c in i.cols.all:
         if c.txt[-1] != "X":
           (i.cols.y if c.txt[-1] in "+-" else i.cols.x).append(c)

  def ydist(i,row)
    return (sum((row[y.pos] - y.goal)**the.p for y in i.cols.y) /len(i.cols.y))**(1/the.p)

  def klassify(i, rows=None)
    rows = i.sorted(rows)
    m = int(len(rows)**0.5)
    n = len(rows) - m
    y = i.ydist(rows[n])
    return lambda r: i.ydist(r) < y, {True:m, False:n}

  def bins(i):
    Y,ys = i.klassify(i.rows),
    for col in i.cols.x:
      bins = sorted(bin for bin in _bins(col, Y, i.rows, ys)):
        
# -----------------------------------------------------------------------------
def show(d):
  if type(d)==str        : return d
  if type(d)==type(show) : return  d.__name__+'()'
  if type(d)==dict:
    return '('+' '.join(f":{k} {show(v)}" for k,v in d.items() if str(k)[0] !="_")+')'
  if type(d)==float: return str(d//1) if d==d//1 else f"{d:.3f}"
  return str(d)

def coerce(s):
  try: return ast.literal_eval(s)
  except: return s.strip()

def csv(f):
  with open(f, "r") as file:
    for line in file.readlines():
      yield [coerce(s) for s in line.split(",")]

def first(l): return l[0]
def second(l): return l[1]

def powerset(nums):
  result = [[]]
  for num in nums:
    result += [subset + [num] for subset in result]
  return result

def fyi(*args, **kwargs):
  say(*args, file=sys.stderr, **kwargs)

def say(*args, **kwargs):
  if the.loud: print(*args, **kwargs)

#------------------------------------------------------------------------------
def cli_csv(_):
  for row in csv(the.train): print(row)

def cli_data(_):
  head,*rows = [r for r in csv(the.train)]
  print(head)
  Y,K = klass(head,rows)
  for i,row in enumerate(sorted(rows, key=Y)):
    if i % 30 == 0: print(i,K(row),row)
  
if __name__== "__main__":
  for j,s in enumerate(sys.argv):
    if todo := vars().get(re.sub("^-","cli_",s)):
      random.seed(the.seed)
      todo( sys.argv[j+1] if j < len(sys.argv) - 1 else None )
