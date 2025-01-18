#!/usr/bin/env python3.13 -B
def cli_h(_): print("\n" + __doc__)

# -----------------------------------------------------------------------------
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

def cli_the(_)    : print(the)
def cli_s(_) : the.loud=False
def cli_r(s) : the.seed=coerce(s); random.seed(the.seed)

# -----------------------------------------------------------------------------
class Num(o):
  def __init__(i,txt=" ",pos=0): 
    i.pos,i.txt,i.n,i.mu,i.m2,i.sd = pos,txt,0,0,0,0
    i.lo, i.hi = BIG, -BIG
    i.goal = 0 if txt[-1] == "-" else 1
    
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
     
  def __repr__(i):
    if i.lo == -BIG : return f"{i.s} <= {i.hi}"  
    if i.hi ==  BIG : return f"{i.s}  > {i.lo}"  
    if i.lo ==  i.hi: return f"{i.s} == {i.lo}"  
    return f"{i.lo} < {i.s} <= {i.hi}"  

# switch whn y 1= y
  def bins(i,Y,rows):
    X    = lambda row: -BIG if row[i.pos]=="?" else row[i.pos]
    rows = sorted(rows, key=X)
    bin  = o(pos=i.pos, lo=i.lo, hi=i.lo, n=0, has={})
    bins = [bin]
    for j,row in enumerate(rows):
      x = row[i.pos]
      bin.y,_ = Y(row)
      if x=="?": continue
      y,support  = Y(row)

      if x != rows[j][i.pos] and bin.n > i.n/the.bins and \
                                 (bin.hi - bin.lo) > i.sd*i.cohen:
        bin.hi = x
        bin    = o(pos=i.pos,lo=x,hi=x,n=0,has={})
        bins  += [bins]
      b.n       += 1
      bin.hi     = x
      bin.has[y] = bin.has.get(y,0) + support
    return i.merge(bins)
     
# -----------------------------------------------------------------------------
class Sym(o):
  def __init__(i,txt=" ",pos=0): 
    i.pos,i.txt,i.n = pos,txt,0
    i.mode, i.most, i.has = 0,0,{}

  def add(i,x):
    if n != "?":
      i.n += 1
      tmp = i.has[x] = i.has.get(x,0) + 1
      if tmp > i.most:
        i.most, i.mode = tmp,x
    return x
           Y
# -----------------------------------------------------------------------------
class DATA(o):
  def __init__(i): 
    i.cols,i.rows = None,[]

  def clone(i):
    return Data().add(i.cols.names)

  def sorted(i, rows=None):
    return (rows or i.rows).sort(key=lambda r: i.ydist(r))

  def adds(i,rows): 
    [i.add(row) for row in rows]    
    return i

  def add(i,row):
    if i.cols: 
       [col.add(row[col.pos]) for col in i.cols.all] 
       i.rows.append( row )
    else:
       x,y,all = [],[],[(Num if s[0].isupper() else Sym)(s,i) for i,s in enumerate(row)]
       for c in all:
         if c.txt[-1] != "X":
           (y if c.txt[-1] in "+-" else x).append(c)
       i.cols = o(names=row, all=all, x=x, y=y)

  def ydist(i,row)
    return (sum((row[y.pos] - y.goal)**the.p for y in i.cols.y) /len(i.cols.y))**(1/the.p)

  def klassify(i, rows=None)
    rows = i.sorted(rows)
    m    = int(len(rows)**0.5)
    n    = len(rows) - m
    y    = i.ydist(rows[m])
    return lambda r: (True,1/m) if i.ydist(r) < y else (False,1/n)

  def bins(i):
    Y = i.klassify(i.rows),
    for col in i.cols.x:
      bins=col.bins(col,i.rows)

      
   
def bridge(bins):
  for i,four in enumerate(bins):
    if i>0:
       bins[i-1][1] = bins[i][0]
  bins[0][0] = -BIG
  bins[-1][1] = BIG
  return bins

def dull(i,j):
  k = {}
  for d in [i,j]:
    for x,n in d.items(): k[x] = k.get(x,0)  + n
  n1,n2 = sum(i.values()), sum(j.values())
  if ent(k) <= (n1*ent(i) + n2*ent(j))/(n1+n2): return k

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

def ent(d):
 N = sum(d.values())
 return - sum(n/N * log(n/N,2) for n in d.values())

def powerset(nums):
  result = [[]]
  for num in nums:
    result += [subset + [num] for subset in result]
  return result

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
