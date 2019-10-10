#!/usr/bin/env python3
# vim: nospell:sta:et:sw=2:ts=2:sts=2

import re,sys

class o:
  def __init__(i,**d) : i.__dict__.update(**d)
  def __repr__(i):
    pairs = [(k, v) for k, v in i.__dict__.items() if k[0] != "_"]
    return i.__class__.__name__ + '{' + ", ".join(
           [('%s=%s' % (k, v)) for k,v in sorted(pairs)]) +'}'

The= o( char = o(no    = "?",
                 less  = "<",  
                 more  = ">",
                 sep   = ",", 
                 doomed= r'([\n\t\r ]|#.*)'),
        dist = o(p=2))

class Tbl(o):
  def __init__(i,names=[], rows=None):
    i.rows = []
    i.cols = o(all=[], decs = [], objs=[])
    if names: i.header(names)
    if rows:  i.read(rows)
  def header(i,names):
    for n,s in enumerate(names):
      x     = (Num    if s[0] in "<>$" else Sym)(s,n)
      what  = i.cols.objs if s[0] in '<>'  else i.cols.decs
      what += [x]
      i.cols.all += [x]
  def read(i,rows):
    for row in rows:
      i + row if i.cols.all else i.header(row)
  def __add__(i,lst):
    lst     = [ c + lst[c.pos] for c in i.cols.all ]
    i.rows += [ Row(lst) ]

class Row(o):
  def __init__(i,lst):
    i.cells=lst
    i.count=0
  def dist(i,j,cols): return dist(i.cells, j.cells. cols)

class Num(o):
  def __init__(i,name="",pos=0):
    i.name,i.pos = name,pos
    i.n,i.mu,i.m2 = 0,0,0
    i.lo,i.hi     = 10**32, -10**32
  def norm(i,x): 
    return (x - i.lo) / (i.hi - i.lo + 10**-32)
  def __add__(i,x):
    if x == The.char.no: return x
    x = float(x)
    if x < i.lo: i.lo = x
    if x > i.hi: i.hi = x
    i.n  += 1
    d     = x - i.mu
    i.mu += d/i.n
    i.m2 += d*(x - i.mu)
    i.sd  = i.sd0()
    return x
  def sd0(i):
    if i.m2 < 0: return 0
    if i.n  < 2: return 0
    return (i.m2/(i.n - 1 + 10**-32))**0.5
  def dist(i,x,y):
    no = The.char.no
    if   x==no and y == no: return 1
    if   x==no: y = i.norm(y); x = 0 if y > 0.5 else 1
    elif y==no: x = i.norm(x); y = 0 if x > 0.5 else 1
    else                     : x,y = i.norm(x), i.norm(y)
    return x-y

class Sym(o):
  def __init__(i,name="",pos=0):
    i.name,i.pos = name,pos
    i.n,i.most,i.mode,i.bag = 0,0,None,{}
  def __add__(i,x):
    if x == The.char.no: return x
    i.n += 1
    c = i.bag[x] = i.bag.get(x,0) + 1
    if c > i.most:
       i.most, i.mode = c, x
    return x
  def dist(i,x,y):
    if   x==no and y == no: return 1
    return 0 if x == y else 1

def dist(x,y,cols, p=The.dist.p):
  s = sum( c.dist( x[c.pos], y[c.pos] )**p for c in cols )
  return s**(1/p) / len(cols)**(1/p)

def err(a,b):
  sys.stderr.write(("#E> "+a+"\n")%b)

def csv(file):
  use, txt = [], ""
  with open(file) as fs:
    for line in fs:
      txt += re.sub(The.char.doomed, '', line)
      if txt and txt[-1] != The.char.sep:
        lst = txt.split(The.char.sep) 
        if lst:
          txt = ""
          use = use or [n for n,s in enumerate(lst) 
                       if s[0] != The.char.no ]
          if len(lst) != len(use):
            err("wanted %s cells, got %s in %s", 
                (len(use), len(lst),lst))
          yield [lst[n] for n in use]

if __name__ == "__main__":
  t =  Tbl(rows = csv("../data/weather.csv")) 
  print(t.cols.objs)
