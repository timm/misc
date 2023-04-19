#!/usr/bin/env python3 -B
# vim: set ts=2 sw=2 et:
"""
twos.py: multi-objective explanation, optimization
(c) 2023, Tim Menzies, <timm@ieee.org>  BSD-2

USAGE:
   ./twos.py [OPTIONS] [-g ACTION]

OPTIONS:
  -g --go    start up action    = nothing
  -h --help  show help          = False
  -s --seed  random number seed = 1234567891
"""
from dataclasses import dataclass as rec
from typing import Dict,Any,List
from termcolor import colored
from copy import deepcopy
import random,math,ast,sys,re

the= {m[1]:m[2] for m in re.finditer(r"\n\s*-\w+\s*--(\w+)[^=]*=\s*(\S+)",__doc__)}

@rec
class BIN():
  lo =  1E60
  hi = -1E60
  n  = 0
  ys =  {}
  #-------------
  def add(i,x,y):
    i.n += 1
    i.lo = min(i.lo,x)
    i.hi = max(i.hi,x)
    i.ys[y] = 1 + i.ys.get(y,0)

@rec
class col():
  at:int  = 0
  txt:str = " "
  bins    = {}
  n:int   = 0
  #-------------
  def add(i,x,inc=1):
    if x=="?": return x
    i.n += inc
    i.add1(x,inc)
  def bin(i,x,y):
    k = i.bin1(x)
    if not k in i.bins: i.bins[k] = BIN(i.at,i.txt,x)
    i.bins[k].add(x)

@rec
class NUM(col):
  w:int    = 1
  mu:float = 0
  m2:float = 0
  sd:float = 0
  #-------------
  def mid(i): return i.mu
  def div(i): return i.sd
  def bin1(i,x):
    z = (x-i.mu) / (i.sd + 1E-60)
    for cut in [-1.28,-.84,-.52,-.25,0,.25,.52,.84,1.28]:
      if z < cut: return cut
    return 1.28
  def add1(i,x,n):
    d     = x - i.mu
    i.mu += d/i.n
    i.m2 += d*(x - col.mu)
    i.sd  = 0 if i.n<2 else (i.m2/(i.n - 1))**.5

@rec
class SYM(col):
  has  = {}
  mode = None
  most = 0
  #-------------
  def mid(i): return i.mode
  def div(i): return -sum((n/i.n*math.log(n/i.n,2) for n in i.has.values() if n > 0))
  def bin1(i,x): return x
  def add1(i,x,inc):
    tmp = i.has[x] = inc + i.has.get(x,0)
    if tmp > i.most: i.most,i.mode = tmp,x

def COL(at=0,txt=" "):
  w = -1 if txt[-1] == "-" else 1
  return NUM(at,txt,w=w) if txt[0].isupper() else SYM(at,txt)

@rec
class ROW:
  cells: list = None

@rec
class DATA:
  x     = []
  y     = []
  cols  = []
  names = []
  rows  = []
  #-------------
  def clone(i,rows=[]):
    return DATA().add(i.names + rows)
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
           (i.x if col.txt[-1] in "+-" else i.y).append(col)
    return i

#-------------------------
class DICT(dict):
  __getattr__ = dict.get
  __setattr__ = dict.__setitem__

def yell(c,*s):
  print(colored(''.join(s),"light_"+c,attrs=["bold"]),end="")

def coerce(x):
  try   : x = ast.literal_eval(x)
  except: pass
  return x

def egs(the):
  for k,v in the.items(): the[k] = cli(k,v)
  print(the.seed)
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
  yell("red","FAIL\n") if tmp==False else yell("green", "PASS\n")
  for k,v in b4.items(): the[k]=v
  return 1 if tmp==False else 0

#-------------------------
class Egs:
  def aa(): print(1,end=" ")

  def they(): print(str(the)[:30],"...",end=" ")
#-------------------------
the = DICT(**{k:coerce(v) for k,v in the.items()})

if __name__ == "__main__": egs(the)
