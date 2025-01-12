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

def of(s,*fn): 
  for f in fn:
    if not f(s): return False
  return True

def usep(s) : return s[-1] != "X"
def xp(s)   : return s[-1] not in "+-!"
def yp(s)   : return not xp(s)
def nump(s) : return s[0].isupper()
def symp(s) : return not nump(s)
def nums(n) : return 1E-32 if n=="?" else n

def merges(lst, eps,nough,  out=None):
  def grow(x, lo,hi,n):
    if n < eps and (hi - lo) < nough: return (lo,x,n+1)

  for x in lst:
   if x != "?":
     if out:
        if it := grow(x, *out[-1]): out[-1] = it
        else                      : out += [(x,x,1)]
     else: out = [(x,x,1)]
  out 

def eg_one(_): 
  head,*rows = [r for r in csv(the.train)]
  for i,h in enumerate(head):
    if of(h, usep,xp,nump):
       print(*[r[i] for r in sorted(rows, key=lambda r: nums(r[i]))]) 

if __name__== "__main__":
  random.seed(the.seed)
  for j,s in enumerate(sys.argv):
    if todo := vars().get(re.sub("^--","eg_",s)):
      todo(sys.argv[j+1] if j < len(sys.argv) - 1 else None)
