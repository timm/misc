#!/usr/bin/env python3 -B
# vim: set ts=2 sw=2 et:
"""
hap.py: multi-objective explanation, optimization
(c) 2023, Tim Menzies, <timm@ieee.org>  BSD-2

USAGE:
   ./twos.py [OPTIONS] [-g ACTION]

OPTIONS:
  -f  --file  data csv file                       = ../../../4src/data/auto93.csv
  -g  --go    start up action                     = nothing
  -h  --help  show help                           = False
  -m  --min   on N items, recurse down to N**min  = .5
  -r  --rest  expand to len(list)*rest            = 4
  -s  --seed  random number seed                  = 1234567891
"""
from functools import cmp_to_key as cmp2key
from dataclasses import dataclass as rec
from typing import Dict, Any, List
from termcolor import colored
from copy import deepcopy
import random, math, ast, sys, re

the= {m[1]:m[2] for m in re.finditer(r"\n\s*-\w+\s*--(\w+)[^=]*=\s*(\S+)",__doc__)}
#-----------------------------------------------------------------------------
class ID(object):
  def __hash__(i): return hash(id(i))

@rec(unsafe_hash=True)
class BIN():
  lo:float =  1E60
  hi:float = -1E60
  n:int  = 0
  ys:dict[str:int] =  None
  #-------------
  def add(i,x,y):
    i.n += 1
    i.lo = min(i.lo,x)
    i.hi = max(i.hi,x)
    i.ys[y] = 1 + i.ys.get(y,0)

bs=[BIN() for _ in range(10)]
x=set(bs)
[x.add(b) for b in bs]
print(len(x))

#-----------------------------------------------------------------------------
@rec
class col(object):
  at:int  = 0
  txt:str = " "
  bins:dict    = None
  n:int   = 0
  #-------------
  def adds(i,lst): [i.add(x) for x in lst]; return i

  def add(i,x,inc=1):
    if x=="?": return x
    i.n += inc
    i.add1(x,inc)

  def bin(i,x,y):
    k = i.bin1(x)
    if not k in i.bins: i.bins[k] = BIN(i.at,i.txt,x)
    i.bins[k].add(x)

def COL(at=0,txt=" "):
  w = -1 if txt[-1] == "-" else 1
  return NUM(at,txt,w=w) if txt[0].isupper() else SYM(at,txt)
#-----------------------------------------------------------------------------
@rec
class NUM(col):
  w:int    = 1
  mu:float = 0
  m2:float = 0
  sd:float = 0
  #-------------
  def mid(i): return i.mu
  def div(i): return i.sd
  def norm(i,x): return x if x=="?" else (x - i.lo) / (i.hi - i.lo + 1E-60)
  def stats(i,div=False,rnd=2): return round(i.div() if div else i.mid(), rnd)

  def bin1(i,x):
    z = (x-i.mu) / (i.sd + 1E-60)
    for cut in [-1.28,-.84,-.52,-.25,0,.25,.52,.84,1.28]:
      if z < cut: return cut
    return 1.28

  def add1(i,x,n):
    d     = x - i.mu
    i.mu += d/i.n
    i.m2 += d*(x - i.mu)
    i.sd  = 0 if i.n<2 else (i.m2/(i.n - 1))**.5
#-----------------------------------------------------------------------------
@rec
class SYM(col):
  has:dict= None
  mode:Any = None
  most:int = 0
  #-------------
  def mid(i): return i.mode
  def div(i): return -sum((n/i.n*math.log(n/i.n,2) for n in i.has.values() if n > 0))
  def stats(i, div=False, **_) : return i.div() if div else i.mid() 
  def bin1(i,x): return x

  def add1(i,x,inc=1):
    i.has = i.has or {}
    tmp = i.has[x] = inc + i.has.get(x,0)
    if tmp > i.most: i.most,i.mode = tmp,x

#-----------------------------------------------------------------------------
@rec
class ROW(object):
  cells: list = None
  #-----------------
  def better(i,j,data):
    s1, s2, cols, n = 0, 0, data.cols.y, len(data.cols.y)
    for col in cols:
      a,b  = col.norm(i.cells[col.at]), col.norm(j.cells[col.at])
      s1  -= math.exp(col.w * (a - b) / n)
      s2  -= math.exp(col.w * (b - a) / n)
    return s1 / n < s2 / n
#-----------------------------------------------------------------------------
@rec
class DATA(object):
  x     = []
  y     = []
  cols  = []
  names = []
  rows  = []
  #-------------------
  def clone(i,rows=[]):
    d= DATA().add(ROW(i.names))
    [d.adds(row) for row in rows]
    return d

  def read(i,file):
    with open(file) as fp:
      for line in fp:
        line = re.sub(r'([\n\t\r"\' ]|#.*)', '', line)
        if line:
          i.add(ROW(cells= [coerce(s.strip()) for s in line.split(",")]))
    return i

  def add(i,row):
    if i.x:
      [col.add(row.cells[col.at]) for cols in [i.x, i.y] for col in cols]
      i.rows += [row]
    else:
      i.names = row.cells
      i.cols = [COL(at=i,txt=s) for i,s in enumerate(i.names)]
      for col in i.cols:
        if col.txt[-1] != "X":
           (i.y if col.txt[-1] in "+-" else i.x).append(col)
    return i

  def stats(i, cols=None, div=False, rnd=2):
    return DICT(N=len(i.rows), **{col.txt: col.stats(div=div, rnd=rnd) 
                                  for col in (cols or i.y)})  

  def betters(i):
    rows = sorted(i.rows, key=cmp2key(lambda: r1.better(r2,i)))
    cut = len(rows) - int(len(rows))**the.min
    best,rest = [],[]
    for i,row in enumerate(rows):
      row.y = i > cut
      (best if i > cut else rest).append(row)
    return i.clone(best), i.clone(random.sample(rest, len(best)*the.rest)) 
#-----------------------------------------------------------------------------
def showd(d): return "{"+(" ".join([f":{k} {show(v)}"
                         for k,v in sorted(d.items()) if k[0]!="_"]))+"}"

def show(x):
  if callable(x)         : return x.__name__+'()'
  if isinstance(x,float) : return f"{x:.2f}"
  return x

class DICT(dict):
  __getattr__ = dict.get
  __setattr__ = dict.__setitem__
  __repr__    = showd

def prin(*l) : print(*l,end="")
def round2(x): return round(x, ndigits=2)

def yell(c,*s):
  print(colored(''.join(s),"light_"+c,attrs=["bold"]),end="")

def coerce(x):
  try   : x = ast.literal_eval(x)
  except: pass
  return x

def egs(the):
  for k,v in the.items(): the[k] = cli(k,v)
  sys.exit(sum([eg(s,the) for s in dir(Egs) if s[0] !="_" and (the.go=="." or the.go==s)]))

def cli(k,v):
  v = str(v)
  for i,x in enumerate(sys.argv):
    if ("-"+k[0]) == x:
      v= "False" if v=="True" else ("True" if v=="False" else sys.argv[i+1])
  return coerce(v)

def eg(name, the):
  b4 = {k:v for k,v in the.items()}
  f  = getattr(Egs,name," ")
  yell("yellow","# ",name," ")
  random.seed(the.seed)
  tmp = f()
  yell("red"," FAIL\n") if tmp==False else yell("green", " PASS\n")
  for k,v in b4.items(): the[k]=v
  return 1 if tmp==False else 0

#-------------------------
class Egs(object):
  def they(): print(str(the)[:30],"...",end=" ")
  
  def num():
    num = NUM().adds(random.random() for _ in range(10**3))
    return .28 < num.div() < .3 and .49 < num.mid() < .51

  def sym():
    sym = SYM().adds("aaaabbc")
    print(sym)
    return 1.37 < sym.div() < 1.39 and sym.mid()=='a'

  def read():
    prin(DATA().read(the.file).stats())

  def betters():
    prin(DATA().betters())

#-------------------------
the = DICT(**{k:coerce(v) for k,v in the.items()})
if __name__ == "__main__": egs(the)
