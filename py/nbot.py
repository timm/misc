#!/usr/bin/env python3
# vim: nospell:sta:et:sw=2:ts=2:sts=2

import re,sys
import sys

no = "?"

def err(a,b):
  sys.stderr.write(("#E> "+a+"\n")%b)

class Num:
  def __init__(i,name="",pos=0,inits=[]):
    i.name,i.pos = name,pos
    i.n,i.mu,i.m2 = 0,0,0
    i.lo,i.hi     = 10**32, -10**32
    [i + x for x in inits]
  def prep(i,x): 
    return float(x)
  def norm(i,x): 
    return (x - i.lo) / (i.hi - i.lo + 10**-32)
  def __add__(i,x):
    if x < i.lo: i.lo = x
    if x > i.hi: i.hi = x
    i.n  += 1
    d     = x - i.mu
    i.mu += d/i.n
    i.m2 += d*(x - i.mu)
    i.sd = i.sd0()
    return x
  def sd0(i):
    if i.m2 < 0: return 0
    if i.n  < 2: return 0
    return  (i.m2/(i.n - 1 + 10**-32))**0.5
  def dist(i,x,y):
    if   x==no and y == no: return 1
    if   x==no: y = i.norm(y); x = 0 if y > 0.5 else 1
    elif y==no: x = i.norm(x); y = 0 if x > 0.5 else 1
    else x,y = i.norm(x), i.norm(y)
    return (x-y)^2

class Sym:
  def __init__(i,name="",pos=0,inits=[]):
    i.name,i.pos = name,pos
    i.n,i.most,i.mode,i.bag = 0,0,None,{}
    [i + x for x in inits]
  def prep(i,x): return x
  def norm(i,x): return x
  def __add__(i,x):
    i.n += 1
    c = i.bag[x] = i.bag.get(x,0) + 1
    if c > i.most:
       i.most, i.mode = c, x
    return x
  def dist(i,x,y):
    if   x==no and y == no: return 1
    return 0 if x == y else 1

class Tbl:
   def __init__(i):
     i.rows = []
     i.decs = []
     i.objs = []

class Row:
  def __init__(i,lst):
    i.cells=lst
    i.count=0
  def dist(i,j,cols):
    return dist(i.cells, j.cells. cols)

def dist(x,y,cols)
  s = sum( c.dist( x[c.pos], y[c.pos] ) for c in cols )
  return s**0.5 / len(cols)**0.5

def egs(file, less  = "<",  more=">",
             sep = ",", doomed = r'([\n\t\r ]|#.*)'):
  want = lambda z: z[0] != no
  t= Tbl()
  cols = None

  def ako(name,n,x):
    try: return int(x) and Num(name, n)
    except:
      try: return float(x) and Num(name, n)
      except ValueError: return Sym(name, n)

  def prep(n,x):
    if x is no: return x
    f = cols[n] = cols[n] or ako(names[n],n,x)
    try: return f + f.prep(x)
    except ValueError:
      err("wanted %s in col %s, got [%s]", ( f.__name__,n,x))

  def cols(src):
    for lst in src:
      if cols:
        t.rows += [ Row([prep(n,x) for n,x in enumerate(lst)]) ]
      else:
        cols = [None for _ in lst]
        names = lst
    for col in cols:
      if   col.name[0] == less: t.objs += [col]
      elif col.name[0] == more: t.objs += [col]
      else: t.decs += [col]
    return t

  def rows(file):
    use, txt = [], ""
    with open(file) as fs:
      for line in fs:
        txt += re.sub(doomed, '', line)
        if txt and txt[-1] != sep:
          lst = txt.split(sep) 
          if lst:
            txt = ""
            use = use or [n for n,s in enumerate(lst) if want(s)] 
            if len(lst) != len(use):
              err("wanted %s cells, got %s in %s", (len(use), len(lst),lst))
            yield [lst[n] for n in use]
  # --------------------------------------------------------
  return cols( rows(file)) 

if __name__ == "__main__":
  t =  egs("../data/weather.csv") 
  print(t.objs[-1].sd)
