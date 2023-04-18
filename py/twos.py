import ast
import math
from dataclasses import dataclass as rec
from typing import Dict,Any

@rec
class col():
  at:int  = 0
  txt:str = " "
  bins    = {}
  n:int   = 0
  def add(i,x,inc=1):
    if x=="?": return x
    i.n += inc
    i.add1(x,inc)
    k = i.bin1(x)
    if not k in i.bins: i.bins[k] = BIN(i.at,i.txt,x)
    i.bins[k].add(x)

@rec
class NUM(col):
  w:int    = 1
  mu:float = 0
  m2:float = 0
  sd:float = 0
  def add1(i,x,n):
    d     = x - i.mu
    i.mu += d/i.n
    i.m2 += d*(x - col.mu)
    i.sd  = 0 if i.n<2 else (i.m2/(i.n - 1))**.5
  def bin1(i,x):
    z = (x-i.mu) / (i.sd + 1E-60)
    for cut in [-1.28,-.84,-.52,-.25,0,.25,.52,.84,1.28]:
      if z < cut: return cut
    return 1.28

@rec
class SYM(col):
  has  = {}
  mode = None
  most = 0
  def add1(i,x,inc):
    tmp = i.has[x] = i.has.get(x,0) + inc
    if tmp > i.most: i.most,i.mode = tmp,x
  def bin1(i,x): return x
def COL(at=0,txt=" "):
  w = -1 if txt[-1] == "-" else 1
  return NUM(at,txt,w=w) if txt[0].isupper() else SYM(at,txt)

def COLS(names):
  x,y,cols = [],[],[COL(at=i,txt=s) for i,s in enumerate(names)]
  for col in cols:
    if col.txt[-1] != "X":
       (x if col.txt[-1] in "+-" else y).append(col)
  return names,cols,x,y

@rec
class DATA:
  x     = []
  y     = []
  cols  = []
  names = []
  rows  = []
  def clone(i): 
    return DATA().add(i.names)
  def read(i,file):
    with open(file) as fp:
      for line in fp:
        line = re.sub(r'([\n\t\r"\' ]|#.*)', '', line)
        if line:
          i.all([coerce(cell.strip()) for cell in line.split(",")])
  def add(i,row):
    if x: 
      [col.add(row[col.at]) for cols in [i.x, i.y] for col in cols]
    else: 
      i.names,i.cols,i.x, i.y = COLS(row)
    return i

@rec
class BIN():
  lo =  1E60
  hi = -1E60
  ys = SYM()
  def add(i,x):
    i.lo = min(i.lo,x)
    i.hi = max(i.hi,x)
    i.ys[x] = 1 + i.ys.get(x,0)

def coerce(x):
  if x=="?": return x
  try: return ast.literal_eval(x)
  except: return x

