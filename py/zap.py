#!/usr/bin/env python3.13 -B

"""
zap.py : fast active learning
(c) 2024 Tim Menzies <timm@ieee.org> MIT license

USAGE:
  lua tiny.py [OPTIONS] [ARG]

OPTIONS:
 --help    : show help
 --the num : asddsas
 --sassads : asd asdas
"""

def eg_help(_): print("\n" + __doc__)

# -----------------------------------------------------------------------------
import random,re,ast,sys
from math import sqrt,log,exp,pi
from typing import Iterable

BIG=1E32

class Obj:
  __init__ = lambda i,**d: i.__dict__.update(d)
  __repr__ = lambda i    : show(i.__dict__)

the = Obj(seed= 1234567891,
          k=1, 
          m=2,
          train="../../moot/optimize/misc/auto93.csv",
          top=5)

def eg_the(_): print(the)

# -----------------------------------------------------------------------------
toggle    = int # -1 or 1
num       = int | float
atom      = num | bool | str # | "?"
row       = list[atom]
rows      = list[row]
Sym,Num   = Obj, Obj
Data,Cols = Obj, Obj
Col       = Num | Sym

# -----------------------------------------------------------------------------
def show(d):
  if type(d)==str        : return d
  if type(d)==type(show) : return  d.__name__+'()'
  if type(d)==dict:
    return '('+' '.join(f":{k} {show(v)}" for k,v in d.items() if str(k)[0] !="_")+')'
  if d==d//1           : return str(d//1)
  if type(d)==float    : return f"{d:g}"
  return str(d)

def coerce(s):
  try: return ast.literal_eval(s)
  except: return s.strip()

def csv(f):
  with open(f, "r") as file:
    for line in file.readlines():
      yield [coerce(s) for s in line.split(",")]

# -----------------------------------------------------------------------------
def Num(txt=" ",at=0) -> Num: 
  return Obj(this=Num, at=at, txt=txt, n=0, mu=0, m2=0, lo=BIG, hi= -BIG,
             goal= 0 if txt[-1]=="-" else 1)

def Sym(txt=" ",at=0) -> Sym: 
  return Obj(this=Sym, at=at, txt=txt, n=0, has={}) 

def Data(src: Iterable[row]) -> Data:
  i = Obj(rows=[], cols = None)
  for row in src:
    if    i.cols: addData(i,row)
    else: i.cols = Cols(row)
  return i

def clone(i: Data, rows=[]) -> Data:
  return Data([i.cols.names] + rows)

def Cols(names: list[str]) -> Cols:
  all,x,y = [],[],[]
  for at,s in enumerate(names):
    all += [ (Num if s[0].isupper() else Sym)(s,at) ]
    if s[-1] != "X": 
      (y if s[-1] in "+-!" else x).append(all[-1])
  return Obj(names=names, all=all, x=x, y=y)

# -----------------------------------------------------------------------------
def addData(i:Data,row:row):
  i.rows += [row]
  return addCols(i, row)

def addCols(i:Data,row:row, n:toggle=1):
  [addCol(col,x,n) for col,x in zip(i.cols.all, row) if x != "?"]
  return row

def addCol(i: Col, x:atom, n:toggle=1):
  print(i)
  i.n += n
  if i.this is Sym:
   i.has[x] = i.has.get(x,0) + n
  else:
    if x < i.lo: i.lo = x
    if x > i.hi: i.hi = x
    d     = x - i.mu
    i.mu += n*(d / i.n)
    i.m2 += n*(d * (x - i.mu))
  return i

# shortcuts and demos
add = addCol

def adds(i: Col,lst, n:toggle=1): [addCol(i,x,n) for x in lst]; return i

def eg_load(f: str): d= Data(csv(f)); [print(col) for col in d.cols.x]

def eg_addSub(_):
  num, at = Num(), {x:add(num,x).mu for x in range(20)}
  for x in sorted(at.keys(), reverse=True)[:18] :
    print(x,add(num,x,-1).mu,at[x-1])
# -----------------------------------------------------------------------------
def mid(i:Col):
  return max(i.has, key=i.get) if i.this is Sym else i.mu

def div(i:Col):
  if i.this is Sym: 
    return -sum(n/i.n*log(n/i.n) for n in i.has.values() if n > 0) 
  else: 
    return 0 if i.n < 2 else (i.m2/(i.n - 1))**.5

def mids(i:Data): return {col.txt:mid(col) for col in i.cols.all}
def divs(i:Data): return {col.txt:div(col) for col in i.cols.all}

def ydist(i:Data,row):
  return sqrt(sum(abs(norm(col,row[col.at]) - col.goal)**2 for col in i.cols.y))

def norm(i:Num,x:num) -> num: # 0..1
  return x if x=="?" else (x - i.lo) / (i.hi - i.lo + 1/BIG)

# -----------------------------------------------------------------------------
def like(i,x,prior):
  if i.this is Sym:
    return (i.has.get(x,0) + the.m*prior) / (i.n + the.m)
  else:
    v   = div(i)**2 + 1/BIG
    tmp = exp(-1*(x - mid(i))**2/(2*v)) / (2*pi*v) ** 0.5
    return max(0,min(1, tmp + 1/BIG)) 

def likes(i,row, nall,nh):
  prior = (len(i.rows) + the.k) / (nall + the.k * nh)
  tmp   = [like(col, row[col.at], prior) for col in i.cols.x]
  return log(prior) + sum(log(x) for x in tmp if x > 0)

def eg_like(f):
   d= Data(csv(f))
   for row in sorted(d.rows, key=lambda r: likes(d,r,1000,2))[::30]:
       print(likes(d,row,1000,2))

def bestish(row,best,rest):
  nall= len(best.rows) + len(rest.rows)
  b= likes(best,row, nall, 2)
  r= likes(rest,row, nall, 2)
  return b - r

def zappy(i):
  lives,maybe,yes,done = 5,0,0, clone(i, i.rows[:the.top])
  Y = lambda row: ydist(done,row)
  N = lambda : int(sqrt(len(done.rows)))
  done.rows.sort(key=Y)
  best,rest = clone(done, done.rows[:N()]), clone(done, done.rows[N():])
  for row in i.rows[the.top:]:
    adds(done, row)
    if bestish(row,best,rest) > 0:
      maybe += 1
      if y(row) < y(best.rows[-1]):
        lives = 5
        yes += 1
        adds(best, row)
        best.rows.sort(key=Y)
        best.rows = tmp[:N()]
        print(tmp[N()])
        [addData(rest, addCols(best, row, -1)) for row in tmp[N():]]
        continue
    lives -= 1
    adds(rest, row)
    if lives == 0: break
  best.rows.sort(key=Y)
  rest.rows.sort(key=br)
  return best,rest

def eg_zap(f):
   zappy(Data(csv(f)))

# -----------------------------------------------------------------------------
if __name__== "__main__":
  for j,s in enumerate(sys.argv):
    if todo := vars().get(re.sub("^--","eg_",s), None):
      todo(sys.argv[j+1] if j < len(sys.argv) - 1 else None)
