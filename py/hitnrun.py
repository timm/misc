#!/usr/bin/env python3 -B
# vim : set et sw=2 ts=2 :
from fileinput import FileInput as file_or_stdin
import re,sys,ast,math,random,inspect,datetime
from math import exp,log,cos,sqrt,pi
from time import time
R=random.random

#------------------------------------------------------------------------------
class o:
  def __init__(i,**d): i.__dict__.update(d)
  def __repr__(i)    : return i.__class__.__name__ + show(i.__dict__)

the = o(k=1, m=2,  train="../data/auto93.csv",seed=123457891)

#------------------------------------------------------------------------------
class COL(o): 
  def __init__(i,at=0,txt=" "): 
    i.at=0; i.txt=" "; i.n=0

  def adds(i,lst): [i.add(x) for x in lst]; return i

#------------------------------------------------------------------------------
class SYM(COL): 
  def __init__(i,**d): 
    super().__init__(**d)
    i.most=0; i.mode=None; i.has={}

  def add(i,x):
    i.n += 1
    i.has[x] = i.has.get(x,0) + 1
    if i.has[x] > i.most: i.most,i.mode = i.has[x], x

  def like(i, x, prior): return (i.has.get(x,0) + the.m*prior) / (i.n + the.m)

  def sub(i,x):
    i.n      -= 1
    i.has[x] -= 1

#------------------------------------------------------------------------------
class NUM(COL): 
  def __init__(i,**d): 
    super().__init__(**d)
    i.mu=0; i.m2=0; i.sd=0; i.lo=1E32; i.hi= -1E32
    i.goal = 0 if i.txt and i.txt[-1] == "-" else 1

  def add(i,x):
    i.n  += 1 
    d     = x - i.mu
    i.mu += d/i.n
    i.m2 += d * (x-i.mu)
    if x  > i.hi: i.hi = x 
    if x  < i.lo: i.lo = x 
    i.sd  = 0 if i.n < 2 else (i.m2/(i.n - 1))**.5

  def like(i, x, _):
    v     = i.sd**2 + 1E-30
    nom   = exp(-1*(x - i.mu)**2/(2*v)) + 1E-30
    denom = (2*pi*v) **0.5
    return min(1, nom/(denom + 1E-30))

  def sub(i,x):
    i.n  -= 1
    d     = x - i.mu
    i.mu -= d/i.n
    i.m2 -= d*(x - i.mu) 
    i.sd  = 0 if i.x < 2 else (i.m2/(i.n - 1))**.5

#------------------------------------------------------------------------------
class COLS:
  def __init__(i,names):
    i.names, i.x, i.y, i.all = names,[],[],[]
    for at,txt in enumerate(names):
      a,z = txt[0],txt[-1]
      col = (NUM if a.isupper() else SYM)(at=at, txt=txt)
      i.all.append(col)
      if z != "X":
        (i.y if z in "!+-" else i.x).append(col)
        if z=="!": i.klass = col
        if z=="-": col.goal = 0

  def add(i, row):
    [col.add(row[col.at]) for cols in [i.x, i.y] for col in cols if row[col.at] != "?"]
    return row

#------------------------------------------------------------------------------
class DATA:
  def __init__(i): i.rows=[]; i.cols=None

  def clone(i,rows):
    return DATA().add(i.cols.names).adds(rows)

  def add(i,row):
    if     i.cols: i.rows += [i.cols.add(row)]
    else:  i.cols = COLS(row)

  def csv(i,fname):
   infile = sys.stdin if fname=="-" else open(fname)
   with infile as src:
     for line in src:
      line = re.sub(r'([\n\t\r ]|#.*)', '', line)
      if line: 
        i.add([coerce(s) for s in line.split(",")])
   random.shuffle(i.rows)
   return i

  def adds(i,rows): [i.add(row) for row in rows]; return i

  def loglike(i, row, nall, nh):
    prior = (len(i.rows) + the.k) / (nall + the.k*nh)
    likes = [c.like(r[c.at], prior) for c in i.cols.x if r[c.at] != "?"]
    return sum(log(x) for x in likes + [prior] if x>0)

#------------------------------------------------------------------------------
def coerce(x):
  try             : return ast.literal_eval(x)
  except Exception: return x

def normal(mu,sd):
 while True:
   x1 = 2.0 * R() - 1
   x2 = 2.0 * R() - 1
   w = x1*x1 + x2*x2
   if w < 1: return mu + sd * x1 * sqrt((-2*log(w))/w)

def show(x):
  if isinstance(x,float): return f"{x:g}"
  if not isinstance(x,dict): return str(x)
  return "{"+' '.join([f":{k} {show(x[k])}" for k in x if k[0] != "_"])+"}"


#------------------------------------------------------------------------------
class eg:
  def noop():  pass
  def one(): print(1)
  def num(): 
    n = NUM().adds([normal(10,1) for x in range(100)]); print(f"{n.mu:g} {n.sd:g}")

  def data():
    d=DATA().csv(the.train)
    print(d.cols.y[0])

random.seed(the.seed)
[getattr(eg, s[1:], eg.noop)() for s in sys.argv]
