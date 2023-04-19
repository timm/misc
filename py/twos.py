import sys
import ast
import math
from dataclasses import dataclass as rec
from typing import Dict,Any,List

@rec
class BIN():
  lo =  1E60
  hi = -1E60
  ys =  {}
  def add(i,x,y):
    i.lo = min(i.lo,x)
    i.hi = max(i.hi,x)
    i.ys[y] = 1 + i.ys.get(y,0)

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
  def clone(i): return DATA().add(i.names)
  def read(i,file):
    with open(file) as fp:
      for line in fp:
        line = re.sub(r'([\n\t\r"\' ]|#.*)', '', line)
        if line:
          i.add(ROW(cells= [coerce(cell.strip()) for cell in line.split(",")]))
    return i
  def add(i,row):
    if x: 
      [col.add(row.cells[col.at]) for cols in [i.x, i.y] for col in cols]
    else: 
      i.names = row.cells
      i.cols = [COL(at=i,txt=s) for i,s in enumerate(i.names)]
      for col in i.cols:
        if col.txt[-1] != "X": (i.x if col.txt[-1] in "+-" else ii.y).append(col)
    return i

def coerce(x):
  if x=="?": return x
  try: return ast.literal_eval(x)
  except: return x

#-------------------------
def okAA(): print(1)

def runs(todo):
  fails=0
  for k,fun in locals().items():
    if k[:2]=="ok" and (todo==k[2:] or todo=="."):
      fails += 1 if fun()==False else 0
  sys.exit(fails)

if __name__ == "__main__":
  runs(sys.argv[1] if len(sys.argv) == 2 else "nothing")
