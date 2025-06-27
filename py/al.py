#!/usr/bin/env python3 -B
"""
abc.py: tiny acive learning,  multi objective.
(c) 2025 Tim Menzies, <timm@ieee.org>. MIT license

Options:
 -f file   : data name = ../../moot/optimize/misc/auto93.csv
 -s seed   : set random number seed = 123456781
 -F Few    : a few rows to explore = 512
 -p p      : distance calcs: set Minkowski coefficient = 2

Bayes:
 -k k      : bayes hack for rare classes = 1
 -m m      : bayes hack for rare attributes = 2

Active learning:
 -a acq    : xploit or xplore or adapt = xploit
 -A Assume : on init, how many initial guesses? = 4
 -B Build  : when growing theory, how many labels? = 20
 -C Check  : when testing, how many checks? = 5 
 -g guess  : ratio for sampling = 0.5 """

import math, random, sys, re
sys.dont_write_bytecode = True

def atom(s):
  for fn in [int, float]:
    try: return fn(s)
    except: pass
  s = s.strip()
  tmp = s.lower()
  return True if tmp=="True" else (False if tmp=="False" else s)

def csv(file):
  with open(file) as f:
    for s in f:
      if s.strip(): yield [atom(x) for x in s.strip().split(",")]

class o:
  __init__ = lambda i, **d: i.__dict__.update(**d)
  __repr__ = lambda i     : f"{i.__class__.__name__}{vars(i)}"

the = o(**{k:atom(v) 
           for k,v in re.findall(r"-\w+\s*(\w+).*=\s*(\S+)",__doc__)})

#--------------------------------------------------------------------
class Sym(o):
  def __init__(i, inits=[], at=0, txt=""): 
    i.at,i.txt,i.n,i.has=at,txt,0,{}
    [i.add(x) for x in inits]

  def add(i,x,inc=1,_=False): 
    if x!="?": 
      i.n+=inc; i.has[x]=i.has.get(x,0)+inc

  def pdf(i,s, prior=0):
    return (i.has.get(s,0) + the.m*prior) / (i.n + the.m + 1/Num.big)

#--------------------------------------------------------------------
class Num(o):
  big = 1e32
  def __init__(i,inits=[], at=0, txt=""):
    i.at,i.txt,i.n = at,txt,0
    i.mu, i.m2, i.sd = 0,0,0
    i.lo,i.hi = i.big, -i.big
    i.heaven = 0 if txt.endswith("-") else 1
    [i.add(x) for x in inits]

  def norm(i,x): return (x-i.lo)/(i.hi-i.lo+1/Num.big)

  def add(i,x, inc=1,_=False):
    if x!="?": 
      i.n += inc; i.lo=min(i.lo,x); i.hi=max(i.hi,x)
      if inc < 0 and i.n < 2:
        i.sd = i.m2 = i.mu = i.n = 0
      else:
       d     = x - i.mu
       i.mu += inc * (d / i.n)
       i.m2 += inc * (d * (x - i.mu))
       i.sd  = 0 if i.n <= 2 else (max(0,i.m2)/(i.n-1))**.5

  def pdf(i,v,_):
    sd  = i.sd or 1 / Num.big
    var = 2 * sd * sd
    z   = (v - i.mu) ** 2 / var
    return min(1, max(0, math.exp(-z) / (2 * math.pi * var) ** 0.5))

#--------------------------------------------------------------------
class Cols(o):
  def __init__(i,names):
    i.names,i.all,i.x,i.y = names,[],[],[]
    i.klass = None
    for n,t in enumerate(names):
      c = (Num if t[0].isupper() else Sym)(at=n,txt=t)
      i.all.append(c)
      if t[-1] != "X":
        (i.y if t[-1] in "!+-" else i.x).append(c)
        if t[-1] == "!": i.klass = c

#--------------------------------------------------------------------
class Data(o):
  def __init__(i, src=[]): 
    i.n,i.rows,i.cols = 0,[],None
    [i.add(x) for x in src]

  def sub(i,t,zap=False): return i.add(t,-1,zap)

  def add(i,t, inc=1, zap=False):
    if not i.cols: i.cols = Cols(t)
    else:
      i.n += inc
      if inc > 0 : i.rows.append(t)
      elif zap   : i.rows.remove(t) # slow for large lists
      for col in i.cols.all: col.add(t[col.at], inc)
    return t

  def clone(i,rows=[]): return Data([i.cols.names]+rows)

  def like(i, t, nall=100, nh=2):
    prior = (i.n + the.k) / (nall + the.k*nh)
    tmp = [c.pdf(v,prior) for c in i.cols.x if (v:=t[c.at]) != "?"]
    return sum(math.log(n) for n in tmp + [prior] if n>0)

  def ydist(i,r):
    d = sum((c.norm(r[c.at])-c.heaven)**2 for c in i.cols.y)
    return (d/len(i.cols.y))**0.5

  def ydists(i,t=None):
    return  sorted(t or i.rows, key=lambda r: i.ydist(r)) # best at 0

#--------------------------------------------------------------------
def acquire(yes, no, t, nall=100, nh=2):
  b = math.exp(yes.like(t, nall, nh))
  r = math.exp(no.like(t, nall, nh))
  p = nall / the.build
  q = dict(xploit=0, xplor=1).get(the.Acq, 1 - p)
  return (b + r*q) / abs(b*q - r + 1 / Num.big)

# lit, dim = split(rows)
# label and sort lit 
# hot, dull = best(sqrt(lit)), other(lit)
# while not enough labels:
#   guess which row in dim scores highest
#   label hi, add to hot
#   resort hot
#   move worst from hot to dull
# return top hot
def acquires(data, rows):
  random.shuffle(rows)
  dim  = rows[the.Assume:]          # unlabeled pool
  lit  = data.clone(rows[:the.Assume]) # labeled items
  lits = lit.ydists() # sort just using the lit knolwedge
  cut  = round(the.Assume**the.Guess) 
  dull = data.clone(lits[cut:]) # rest
  hot  = data.clone(lits[:cut]) # best

  _score  = lambda t: -acquire(hot, dull, t, len(lit.rows), 2)  
  _ranked = lambda t: sorted(t, key=_score)

  while len(dim) > 2 and len(lit.rows) < the.Build:
    hi, *lo = _ranked(dim[:the.Few * 2])
    dim = lo[:the.Few] + dim[the.Few * 2:] + lo[the.Few:]
    lit.add(hot.add(hi))
    *hot.rows, doomed = lit.ydists(hot.rows) # sort via just the lit 
    dull.add (hot.sub( doomed))
  return o(hot=hot.rows[0],
           lit=lit.rows,
           test=data.ydists(_ranked(dim)[:the.check])[0])

#--------------------------------------------------------------------
def eg_h()    : print(__doc__)
def eg__the() : print(the)
def eg__csv() : [print(t) for t in csv(the.file)]
def eg__sym() : print(Sym("aaaabbc").has)
def eg__num() : print(Num(random.gauss(10,1) for _ in range(1000)).sd)
def eg__data(): print(Data(csv(the.file)).cols.x)

def eg__addSub():
  d1=Data(csv(the.file))
  d2=d1.clone()
  for row in d1.rows:
     d2.add(row)
     if len(d2.rows)==100: 
       mu,sd = d2.cols.x[0].mu,d2.cols.x[0].sd
  for row in d1.rows[::-1]:
    if len(d2.rows)==100: 
      assert abs(d2.cols.x[0].mu/mu) < 1.01
      assert abs(d2.cols.x[0].sd) < 1.01
    d2.sub(row,True)

def eg__bayes():
  data = Data(csv(the.file))
  assert all(-20 <= data.like(t) <=0 for t in data.rows)
  print(sorted([data.like(t) for t in data.rows])[::20])

#--------------------------------------------------------------------
def cli(arg,d):
  for k in d:
    if len(arg)> 1 and arg[1] == k[0]: d[k]=atom(arg)

if __name__ == "__main__":
  for arg in sys.argv:
    cli(arg, the.__dict__)
    if (fn := globals().get(f"eg{arg.replace('-', '_')}")):
      random.seed(the.seed)
      fn()
