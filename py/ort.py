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

def norm(n,lo,hi):
   return (n-lo)/(hi - lo + 1/BIG)

def klass(head,rows):
  ys = {col:[BIG,-BIG,goal] for c,goal in (("-",0),("+",1)) 
                            for col,s in enumerate(head) 
                            if s[-1] == c and s[-1] != "X"}
    def ydist(row):
    d = sum(abs(norm(row[col],lo,hi) - goal)**2 for col,(lo,hi,goal) in ys.items())
    return (d / len(ys))**0.5

  for col,etc in ys.items():
    for row in rows:
      etc[0] = min(etc[0], row[col])
      etc[1] = max(etc[1], row[col])
  return ydist, dist(sorted(rows, key=ydist)[ int(len(rows)**0.5) ])

def data(src, **keys):
  head,*rows = [r for r in src]
  Y,border = klass(head,rows)
  for col,s in enumerate(head): 
    if s[-1] not in "+-X"]:
      xys = sorted([(r[col],Y(r) < border) for r in rows if r[col] != "?"],key=first)
      nums(col,xys,**keys) if  s[0].isupper() else syms(col,sys)

def syms(col,xys):
  ds={}
  for x,y in xys:
    d = ds[y] = ds.get(y,{})
    key = (x,x,col)
    d[key] = d.get(key,0) + 1
  return ds

def nums(col,xys, cohen=0.35, bins=17):
  ten   = len(xys) // 10
  small = ((xys[9*ten][0] - xys[ten][0])/2.56) * cohen
  few   = len(xys) / bins
  x     = xys[0][0]
  b     = (x,x,col)
  bins  = {b:0}
  for i,(x,y) in enumerate(xys):
    if i < len(xys) - few and x != xys[i+1][0] and b[1] - b[0] > small and bins[b] > few:
      if last 
      b1 = (x,x,col)
      if last: then ....
      last = b
      bins[b] = 0
    b[1]  = x
    bins[b] += 1
  return bridge(merges(bins)))

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
