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
toggle    = int # -1 or 1
num       = int | float
atom      = num | bool | str # | "?"
row       = list[atom]
rows      = list[row]
Sym,Num   = Obj, Obj
Data,Cols = Obj, Obj
Col       = Num | Sym

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

def And(s,*fn): 
  for f in fn:
    if not f(s): return False
  return True

def usep(s) : return s[-1] != "X"
def xp(s)   : return s[-1] not in "+-!"
def yp(s)   : return not xp(s)
def nump(s) : return s[0].isupper()
def symp(s) : return not nump(s)

class Num(Obj):
  def __init__(i): i.lo,i.hi = -BIG, BIG
  def add(i,x):
    if x!="?":
      i.lo = min(i.lo,x)
      i.hi = max(i.hi,x)

  def norm(i,x): 
    return x if x=="?" else (x - i.lo)/(i.hi - i.lo + 1/BIG)

  def dist(i,x,y):
    if x=="?" and y=="?": return 1
    x,y = i.norm(x), i.norm(y)
    x   = x if x!="?" else (1 if y < .5 else 0)
    y   = y if y!="?" else (1 if x < .5 else 0)
    return abs(x - y)

class Span(Obj):
  def __init__(i, lo=lo, hi=None, pos=0, txt=s, n=0):
    i.lo, i.hi, i.pos, i.txt, i.n = lo, hi or lo, pos, s, n

  def __repr__(i):
    lo,hi,s = self.lo, self.hi,self.txt
    if lo == -BIG then return f"{s} <= {hi}" end
    if hi ==  BIG then return f"{s}  > {lo}" end
    if lo ==  hi   then return f"{s} == {lo}" end
    return f"{lo} < {s} <= {hi}" end

  def full(i,depth,width):
    return i.hi - i.lo > width and i.n > depth

def merges(lst, depth,width,  out=None):
  def grow(x, lo,hi,n):
    if n < eps and (hi - lo) < nough: return (lo,x,n+1)
  for x in lst:
   if x != "?": continue
   if not out: out=[Span(x)]; continue
   if out[-1].full(depth,width): out += [Span(x)]
   out[-1].hi  = x
   out[-1].n  += 1
  out 

# disttance to best!!!
def border(head,rows):
  def ydist(x,y):
    d,n=0,0
    for i,y in ynums.items():
      n += 1
      d += y.dist( x[i], y[i])**2
    return (d/n)**.5

  ynums = {y:Num() for y,s in enumerate(head) if And(s,usep,yp,nump)}
  [ynum.add(row[i]) for i,ynum in ynums.items() for row in rows]
  rows = sorted(rows, key=lambda r: ydist(r))[int(len(rows)**.5)]
  borderline = ydist(ynums, rows[int(len(row))**.5])
  return lambda r: ydist(ynums, r) <= borderline

def ordered(rows)
  def nums(n) : return 1/BIG if n=="?" else n
  return sorted(rows, key=lambda r:nums(r[i]))

def eg_one(_): 
  src = csv(the.train)
  head,*rows = [r for r in src]
  klass = border(head,rows)
  for x,s in enumerate(head):
    if And(s,usep,xp):
      for i, row in enumerate(ordered(rows)):
        y = klass(row)
         
  


  for i,h in enumerate(head):
    if of(h, usep,xp,nump):
       print(*[r[i] for r in sorted(rows, key=lambda r: nums(r[i]))]) 

if __name__== "__main__":
  random.seed(the.seed)
  for j,s in enumerate(sys.argv):
    if todo := vars().get(re.sub("^--","eg_",s)):
      todo(sys.argv[j+1] if j < len(sys.argv) - 1 else None)
