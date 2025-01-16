#!/usr/bin/env python3.13 -B
def eg_help(_): print("\n" + __doc__)

# -----------------------------------------------------------------------------
import random,re,ast,sys
from math import sqrt,log,exp,pi
from typing import Iterable

R=random.random
BIG=1E32

class Obj:
  __init__ = lambda i,**d: i.__dict__.update(d)
  __repr__ = lambda i    : show(i.__dict__)

the = Obj(seed= 1234567891,
          cliffs=0.197,
          boots=512,
          conf=0.05,
          k=1,
          m=2,
          stop=256,
          loud=True,
          train="../../moot/optimize/misc/auto93.csv",
          top=6)

def eg_the(_)   : print(the)
def eg_silent(_): the.loud=False
def eg_seed(s)  : the.seed=coerce(s); random.seed(the.seed)



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

def stdev(l,key=first):
  ten = len(l)//10
  return (key(l[9*ten]) - key(l[ten]))/2.56

def ent(d):
 N = sum(d.values())
 return - sum(n/N * log(n/N,2) for n in d.values())

def powerset(nums):
  result = [[]]
  for num in nums:
    result += [subset + [num] for subset in result]
  return result

# -----------------------------------------------------------------------------
def show(s,lo,hi,*_):
  if lo == -BIG : return f"{s} <= {hi}"  
  if hi ==  BIG : return f"{s}  > {lo}"  
  if lo ==  hi   : return f"{s} == {lo}"  
  return f"{lo} < {s} <= {hi}"  

def klass(head,rows):
  ys = {col:[BIG,-BIG,goal] for c,goal in (("-",0),("+",1)) 
                            for col,s in enumerate(head) 
                            if s[-1] == c and s[-1] != "X"}
    
  def norm(z,lo,hi): 
    return (z-lo)/(hi - lo + 1/BIG)

  def ydist(row):
    d = sum(abs(norm(row[col],lo,hi) - goal)**2 for col,(lo,hi,goal) in ys.items())
    return (d / len(ys))**0.5
  
  for col,etc in ys.items():
    for row in rows:
      etc[0] = min(etc[0], row[col])
      etc[1] = max(etc[1], row[col])
  print(100,ys)
  stop = int(len(rows)**.5)
  border = ydist(sorted(rows, key=ydist)[stop])
  print(stop)
  return ydist,lambda row: ydist(row) <= border

def data(src, **keys):
  head,*rows = [r for r in src]
  Y = klass(head,rows)
  for col,s in enumerate(head):
    if s[-1] not in "+-X":
      xys = [(r[col], Y(r)) for r in rows if r[col] != "?" ]
      return spans(col,xys, **keys) if s[0].isupper() else syms(col,xys)

def syms(col,xys):
  ds={}
  for x,y in xys:
    key = (col,x,x)
    d = ds[y] = ds.get(y,{})
    d[key] = d.get(key,0) + 1
  return ds
    
def spans(col,xys, cohen=0.35, bins=17):
  xys   = sorted(xys, key=first)
  small = stdev(xys, key=first) * cohen
  few   = len(xys) / bins
  x     = xys[0][0]
  b     = (x,x,0,{})
  bins  = [b]
  for i,(x,y) in enumerate(xys):
    if i < len(xys) - few and x != xys[i+1][0] and b[1] - b[0] > small and b[2] > few:
      b = (x,x,0,{})
      bins += [b]
    b[1]  = x
    b[2] += 1
    b[3]  = b[3].get(y,0) + 1
  return count(col,bridge(merges(bins)))

def count(col, fours):
  ds={}
  for lo,hi,_,d in fours:
    for y,n in d.items():
      key=(col,lo,hi)
      d = ds[y] = ds.get(y,{})
      d[key] = d.get(key,0) + 1
  return ds

def bridge(bins):
  for i,four in enumerate(bins):
    if i>0:
       bins[i-1][1] = bins[i][0]
  bins[0][0] = -BIG
  bins[-1][1] = BIG
  return bins

def merges(b4):
  now,i = [],0
  while i < len(b4):
    lo, hi, n, d = b4[i]
    if i < len(b4) - 1:
      __, hi1, ___, d1 = b4[i+1]
      if d3 := merge(d1,d2):
        hi,  d = hi1, d3
        i += 1
    new += [(lo,hi,n,d)]
    i += 1
  return b4 if len(now) == len(b4) else merges(now)

def merge(i,j):
  k = {}
  for d in [i,j]:
    for x,n in d.items():
      k[x] = k.get(x,0)  + n
  n1,n2 = sum(i.values()), sum(j.values())
  if ent(k) <= (n1*ent(i) + n2*ent(j))/(n1+n2): return k

#------------------------------------------------------------------------------
def eg_csv(_):
  for row in csv(the.train): print(row)

def eg_data(_):
  head,*rows = [r for r in csv(the.train)]
  print(head)
  Y,K = klass(head,rows)
  for i,row in enumerate(sorted(rows, key=Y)):
    if i % 30 == 0: print(i,K(row),row)
  
if __name__== "__main__":
  random.seed(the.seed)
  for j,s in enumerate(sys.argv):
    if todo := vars().get(re.sub("^--","eg_",s)):
      todo(sys.argv[j+1] if j < len(sys.argv) - 1 else None)
