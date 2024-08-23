#!/usr/bin/env python3 -B
# vim : set et sw=2 ts=2 :
from fileinput import FileInput as file_or_stdin
from dataclasses import dataclass, field, fields
import datetime
from math import exp,log,cos,sqrt,pi
import re,sys,ast,math,random,inspect
from time import time
R=random.random

class o:
  def __init__(i,**d): i.__dict__.update(d)
  def __repr__(i)    : return str(i.__dict__)

the = o(k=1, m=2,  train="../data/auto93.csv")

@dataclass
class COL(o): 
  def adds(i,lst): [i.add(x) for x in lst]; return i

@dataclass
class SYM(COL): 
  def __init__(i): i.at,i.txt, i.n,i.most,i.mode ==0, txt=" ", n=0, most=0; mode=None; has:DICT()
  def coerce(i, x): return x 
  def add(i,x):
    i.n += 1
    i.has[x] = i.has.get(x,0) + 1
    if i.has[x] > i.most: i.most,i.mode = i.has[x], x

  def sub(i,x):
    i.n      -= 1
    i.has[x] -= 1

@dataclass
class NUM(COL): 
  mu=0; m2=0; sd=0; lo=1E32; hi= -1E32; goal=1

  def __post_init__(i) -> None: i.goal = 0 if i.txt and i.txt[-1] == "-" else 1

  def coerce(i, x): return x if x == "?" else coerce(x) 

  def add(i,x):
    i.n  += 1 
    d     = x - i.mu
    i.mu += d/i.n
    i.m2 += d * (x-i.mu)
    if x  > i.hi: i.hi = x 
    if x  < i.lo: i.lo = x 
    i.sd  = 0 if i.n < 2 else (i.m2/(i.n - 1))**.5

  def sub(i,x):
    i.n  -= 1
    d     = x - i.mu
    i.mu -= d/i.n
    i.m2 -= d*(x - i.mu) 
    i.sd  = 0 if i.x < 2 else (i.m2/(i.n - 1))**.5

  def like(i, x, prior): return (i.has.get(x,0) + the.m*prior) / (i.n + the.m)

  def like(i, x, _):
    v     = i.sd**2 + 1E-30
    nom   = exp(-1*(x - i.mu)**2/(2*v)) + 1E-30
    denom = (2*pi*v) **0.5
    return min(1, nom/(denom + 1E-30))

@dataclass
class COLS:
  names: LIST(); all:LIST(); x:LIST(); y:LIST(); klass= None

  def __post_init__(i):
    for at,txt in enumerate(i.names):
      a,z = txt[0],txt[-1]
      col = (NUM if a.isupper() else SYM)(at, txt)
      i.all.append(col)
      if z != "X":
        (i.y if z in "!+-" else i.x).append(col)
        if z=="!": i.klass = col
        if z=="-": col.goal = 0

  def add(i, row):
    [col.add(row[col.at]) for cols in [i.x, i.y] for col in cols if row[col.at] != "?"]
    return row

@dataclass
class DATA:
  cols:LIST(); rows:LIST()

  def clone(i,rows):
    return DATA().add(i.cols.names).adds(rows)

  def add(i,row):
    if     i.cols: i.rows += [i.cols.add(row)]
    else:  i.cols = COLS(row,[],[],[]) 
    return i

  def reads(i,fname):
   infile = sys.stdin if fname=="-" else open(fname)
   with infile as src:
     for line in src:
      line = re.sub(r'([\n\t\r ]|#.*)', '', line)
      if line: 
        i.add([coerce(s) for s in line.split(",")])

  def adds(i,rows): [i.add(row) for row in rows]; return i

  def loglike(i, row, nall, nh):
    prior = (len(i.rows) + the.k) / (nall + the.k*nh)
    likes = [c.like(r[c.at], prior) for c in i.cols.x if r[c.at] != "?"]
    return sum(log(x) for x in likes + [prior] if x>0)

def coerce(x):
  try             : return ast.literal_eval(x)
  except Exception: return x

def normal(mu,sd):
 while True:
   x1 = 2.0 * R() - 1
   x2 = 2.0 * R() - 1
   w = x1*x1 + x2*x2
   if w < 1: return mu + sd * x1 * sqrt((-2*log(w))/w)

class eg:
  def noop():  pass
  def one(): print(1)
  def num(): 
    n = NUM().adds([normal(10,1) for x in range(100)]); print(f"{n.mu:g} {n.sd:g}")

  def data():
    d=DATA([],[]).reads(the.train)

[getattr(eg, s[1:], eg.noop)() for s in sys.argv]
