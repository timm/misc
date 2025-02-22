#!/usr/bin/env python3 -B
"""
slope.py : active learning, focusing on regions of max dy/dx   
(c) Tim Menzies <timm@ieee.org>, MIT license.   

OPTIONS:   
"""

import re,ast,sys,math,random
from dataclasses import dataclass, field, fields

PYTHONDONTWRITEBYTECODE = False
rand=random.random

num  = float | int
atom = num | bool | str # and sometimes "?"

Big = 1E32

def LIST(): return field(default_factory=list)
def DICT(): return field(default_factory=dict)

@dataclass
class SETTINGS:
  far:float = 0.95
  file:str  = "../data/auto93.csv"
  k:int     = 1
  m:int     = 2
  p:int     = 2
  seed:int  = 1234567891
  start:int = 4
  step:int  = 4
  stop:int  = 100
  xys:int   = 32

the = SETTINGS()

def eg__the(_)  :   
  "show settings"
  print(the)

def eg__seed(n=None): 
  "reset seed      : int"
  the.seed = n or the.seed

def eg_h(_):
  "show help"
  print(__doc__)
  for k,f in sorted(globals().items()):
    if k[:2] == "eg":
      print("    {:10s} {:s}".format(k[2:].replace("_","-"), f.__doc__ or ""))

#------------------------------------------------------------------------------
@dataclass
class ROW:
  cells:list=LIST(); x:int=0; y:int=0

  def at(self,col): return self.cells[col.at]

#------------------------------------------------------------------------------
@dataclass
class COL:
  n:int=0; at:int=9; txt:str=" "

  def add(self, v):
    if v != "?":
      self.n += 1
      self.add1(v)
    return v

  def dist(self, u,v):
    return 1 if u==v=="?" else self.dist1(u,v)

#------------------------------------------------------------------------------
@dataclass
class SYM(COL):
  most:int=0; mode:atom=None; has:dict=DICT()

  def add1(self,v):
    self.has[v] = self.has.get(v,0) + 1
    if self.has[v] > self.most:
      self.mode, self.most = v, self.has[v]

  def dist1(self, u,v): return u != v

  def like(self, v, prior):
    return (self.has.get(v,0) + the.m*prior) / (self.n + the.m)

def eg__sym(_): 
  "test SYMs"
  assert adds("aaaabbc",SYM()).mode == "a"

#------------------------------------------------------------------------------
@dataclass
class NUM(COL):
  mu:num=0; m2:num=0; sd:num=0; lo:num=Big; hi:num=-Big; goal:int=1

  def __post_init__(self):
    if  self.txt and self.txt[-1] == "-": self.goal=0

  def add1(self,v):
    self.lo  = min(v, self.lo)
    self.hi  = max(v, self.hi)
    d        = v - self.mu
    self.mu += d / self.n
    self.m2 += d * (v -  self.mu)
    self.sd  = 0 if self.n <2 else (self.m2/(self.n-1))**.5

  def dist1(self, u,v):
    u, v = self.norm(u), self.norm(v)
    u = u if u !="?" else (1 if v<0.5 else 0)
    v = v if v !="?" else (1 if u<0.5 else 0)
    return abs(u-v)

  def like(self, v, _):
    sd    = self.sd + 1/Big
    nom   = math.exp(-1*(v - self.mu)**2/(2*sd*sd))
    denom = (2*math.pi*sd*sd) ** 0.5
    return max(0, min(1, nom/denom))

  def norm(self,v):
    return v if v=="?" else  ((v - self.lo) / (self.hi - self.lo + 1/Big))

def eg__num(_) :
  "test NUMs"
  N=lambda mu,sd: ((mu or 0) +
                   (sd or 1) * math.sqrt(-2*math.log(rand()))
                             * math.cos(2*math.pi*rand()))
  num1 = NUM()
  for _ in range(10000): num1.add(N(10,2))
  assert abs(10- num1.mu) < 0.05
  assert abs(2 - num1.sd) < 0.05

#------------------------------------------------------------------------------
@dataclass
class COLS:
  names : list[str]
  all   : list[COL] = LIST()
  x     : list[COL] = LIST()
  y     : list[COL] = LIST()
  klass : COL = None

  def __post_init__(self):
    for at,txt in enumerate(self.names):
      a,z = txt[0],txt[-1]
      col = (NUM if a.isupper() else SYM)(at=at, txt=txt)
      self.all.append(col)
      if z != "X":
        (self.y if z in "!+-" else self.x).append(col)
        if z=="!": self.klass = col
        if z=="-": col.goal = 0

  def add(self, row):
    [col.add(row[col.at]) for cols in [self.x, self.y] for col in cols]
    return row

#------------------------------------------------------------------------------
@dataclass
class DATA:
  cols:COLS = None; rows:LIST = LIST()

  def add(self,row):
    if    self.cols: self.rows += [self.cols.add(row)]
    else: self.cols = COLS(names=row)
    return self

  def clone(self, rows=[]):
    return adds(rows, DATA().add(self.cols.names))

  def dist(self, row1, row2):
    tmp = sum(col.dist(row1[col.at], row2[col.at])**the.p for col in self.cols.x)
    return (tmp / len(self.cols.x))**(1/the.p)

  def drop(self,row1,row2):
    return abs(self.ydist(row1) - self.ydist(row2)) / (self.dist(row1,row2) + 1/Big)

  def loglike(self, row, nall=1000, nh=2):
    prior = (len(self.rows) + the.k) / (nall + the.k*nh)
    likes = [col.like(row[col.at], prior) for col in self.cols.x if row[col.at] != "?"]
    return sum(math.log(v) for v in likes + [prior] if v>0)

  def neighbors(self, row1, rows=None):
    return sorted(rows or self.rows, key=lambda row2:self.dist(row1,row2))

  def twoFar(self,rows):
    far   = int(len(rows) * the.far)
    order = lambda two: self.dist(*two)
    A,B   = sorted(((any(rows), any(rows)) for _ in range(the.xys)), key=order)[far]
    return A,B, self.dist(A,B)

  def xys(self,rows=None):
    rows = rows or the.rows
    A,B,c = self.twoFar(rows)
    for C in rows:
      C.x = max(0, min(1, (self.dist(C,A)**2 + c**2 - self.dist(C,B)**2) / (2*c)))
      C.y = (A**2 - C.x**2)**0.5

  def ydist(self, row):
    tmp = sum((col.norm(row[col.at]) - col.goal)**the.p for col in self.cols.y)
    return (tmp / len(self.cols.y))**(1/the.p)


def eg__data(_):
   "test DATA"
   d = adds(csv(the.file),DATA())
   [print(col) for col in d.cols.x]

def eg__ysort(_):
   "sort rows by distance to goal"
   d = adds(csv(the.file),DATA())
   for i,row in enumerate(sorted(d.rows, key=lambda row: d.ydist(row))):
     if i % 30==0 : print(row)

def eg__xdist(_):
   "test distance calcs"
   d = adds(csv(the.file),DATA())
   n = NUM()
   for _ in range(1000):
     n.add( d.dist( any(d.rows), any(d.rows)))
   print({k: rit(n,k,3) for k in "mu sd lo hi n".split()})

def eg__bayes(_):
   "test bayes"
   d = adds(csv(the.file),DATA())
   n = NUM()
   for row in d.rows:
     n.add( d.loglike(row,10000,2))
   print({k: rit(n,k,3) for k in "mu sd lo hi n".split()})

#------------------------------------------------------------------------------
def slope(d):
  rows = shuffle(d.rows)
  done, todo = rows[:the.start], rows[the.start:]
  _,A,B = sorted([(d.drop(r1,r2),r1,r2) for r1 in done for r2 in done if id(r1) > id(r2)],
                   key=of(0), reversed=True)[0]
  c = d.dist(A,B)
  for _ in range(256):
     i = random.randint(0,len(todo)-1)
     tmp += [(abs(d.cos(todo[i],A,B,c) - 0.5),i)]
  done += [ todo.pop( sorted(tmp,key=of(0))[0][1] )]

#------------------------------------------------------------------------------
def adds(it,what=None):
  it=iter(it)
  one = next(it)
  what = what or (NUM if isinstance(one, (float,int)) else SYM)()
  what.add(one)
  [what.add(v) for v in it]
  return what

def coerce(s):
  try: return ast.literal_eval(s)
  except Exception: return s

def csv(file):
  file = sys.stdin if file=="-" else open(file)
  with file as src:
    for line in src:
      line = re.sub(r'([\n\t\r ]|#.*)', '', line)
      if line: yield [coerce(s.strip()) for s in line.split(",")]

def eg__csv(_): 
  "test csv load"
  [print(row) for row in csv(the.file)]

def rand(n=1)    : return n*random.random()
def any(lst)     : return random.choice(lst)
def it(v,k)      : return v.__dict__[k]
def rit(v,k,n)   : return round(it(v,k),n)
def shuffle(lst) : random.shuffle(lst); return lst
def of(n):
  return lambda v: v[n]

def xval(lst, m:int=5, n:int=5, some=10**6):
  for _ in range(m):
    random.shuffle(lst)
    for n1 in range (n):
      lo = len(lst)/n * n1
      hi = len(lst)/n * (n1+1)
      train, test = [],[]
      for i,x in enumerate(lst):
        (test if i >= lo and i < hi else train).append(x)
      train = random.choices(train, k=min(len(train),some))
      test = random.choices(test, k=min(len(test),some))
      yield train,test

#------------------------------------------------------------------------------
if __name__ == "__main__":
  for i,s in enumerate(sys.argv):
    if fun := vars().get("eg" + s.replace("-","_")):
       arg = None if i==len(sys.argv) - 1 else sys.argv[i+1]
       random.seed(the.seed)
       fun(coerce(arg))
