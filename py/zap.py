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

R=random.random
BIG=1E32

class Obj:
  __init__ = lambda i,**d: i.__dict__.update(d)
  __repr__ = lambda i    : show(i.__dict__)

the = Obj(seed= 1234567891,
          cliffs=0.197,
          boots=512,
          conf=0.05,
          k=1, 
          m=2,
          stop=256,
          loud=True,
          train="../../moot/optimize/misc/auto93.csv",
          top=6)

def eg_the(_)   : print(the)
def eg_silent(_): the.loud=False
def eg_seed(s)  : the.seed=coerce(s); random.seed(the.seed)

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
  if type(d)==float: return str(d//1) if d==d//1 else f"{d:.3f}"
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

def adds(i: Col,lst, n:toggle=1): 
  [addCol(i,x,n) for x in lst]; return i

def eg_load(f: str): 
  d= Data(csv(f)); [print(col) for col in d.cols.x]

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
  return b > r

def fyi(x): 
  if the.loud: print(x,end="",file=sys.stderr, flush=True)

def zappy(i):
  evals,done = the.top,clone(i, i.rows[:the.top])
  Y = lambda row: ydist(i,row)
  N = lambda : int(sqrt(len(done.rows)))
  done.rows.sort(key=Y)
  best,rest = clone(done, done.rows[:N()]), clone(done, done.rows[N():])
  tries,yes = 1/BIG,0
  c=""
  for j,row in enumerate(i.rows[the.top:the.stop]):
    fyi(c)
    if j % 100 == 0: fyi("\n")
    addData(done, row)
    c="."
    if R() < 1-j/30 or bestish(row,best,rest):
      evals += 1
      tries += 1
      c="?"
      if Y(row) < Y(best.rows[0]):
        c="!"
        yes += 1
        addData(best, row)
        tmp  = sorted(best.rows, key=Y)
        best = clone(done, tmp[:N()])
        [addData(rest, row) for row in tmp[N():]]
        continue
    if R() < 1-1/len(best.rows): addData(rest, row)
  best.rows.sort(key=Y)
  return best,rest, evals, yes/tries

def eg_zap(f):
   d=Data(csv(f))
   nb4 =adds(Num(), [ydist(d,row) for row in d.rows])
   Y = lambda row: ydist(d,row)
   repeats = 20
   rands,zaps,evals,yess = [],[],[],[]
   for _ in range(repeats):
     random.shuffle(d.rows)
     best,_,evals1,yes= zappy(d)
     yess  += [yes]
     evals += [evals1]
     zaps  += [Y(best.rows[0])]
     rands += [Y(sorted(random.choices(d.rows, k=evals1), key=Y)[0])]
   nyes   = adds(Num(),yess)
   nzaps  = adds(Num(),zaps)
   nrands = adds(Num(),rands)
   nevals = adds(Num(),evals)
   delta  = 100*(nb4.mu - nzaps.mu)/nb4.mu //1
   diff   = (nrands.mu - nzaps.mu)/div(nb4)
   height = (nzaps.mu - nb4.lo)/ div(nb4)
   print(f"{delta}", f"{diff:.2f}", f"{height:.2f}",
         nzaps.mu < nrands.mu and not same(zaps,rands),
         nevals.mu//1, f"{nyes.mu:.2f}",re.sub(".*/","",f))

# -----------------------------------------------------------------------------
def same(y0,z0):
  return cliffs(y0,z0) and bootstrap(y0,z0) 

def cliffs(y0,z0):
  """non-parametric effect size. threshold is border between small=.11 and medium=.28 
  from Table1 of  https://doi.org/10.3102/10769986025002101"""
  n,lt,gt = 0,0,0
  for x1 in y0:
    for y1 in z0:
      n += 1
      if x1 > y1: gt += 1
      if x1 < y1: lt += 1 
  return abs(lt - gt)/n  < the.cliffs

def  bootstrap(y0,z0):
  """non-parametric significance test From Introduction to Bootstrap, 
    Efron and Tibshirani, 1993, chapter 20. https://doi.org/10.1201/9780429246593"""
  DIFF  = lambda i,j: abs(i.mu - j.mu) / ((div(i)**2/i.n + div(j)**2/j.n)**.5 + 1E-30)
  SOME  = lambda lst: adds(Num(), random.choices(lst, k=len(lst))) 
  x,y,z = adds(Num(),y0+z0), adds(Num(),y0), adds(Num(),z0)
  yhat  = [y1 - y.mu + x.mu for y1 in y0]
  zhat  = [z1 - z.mu + x.mu for z1 in z0] 
  n     = sum(DIFF(SOME(yhat), SOME(zhat)) > DIFF(y,z) for _ in range(the.boots)) 
  return n / the.boots >= the.conf

def eg_stats(_):
  y =lambda s: "y" if s else "."
  g = random.gauss
  d,r= 1,100
  print("d\tclif\tboot\tcohen")
  while d< 1.2:
    t = [g(10,1) + g(10,2)**2 for _ in range(r)]
    u = [x*d for x in t]
    d = d*1.01
    n1,n2 = adds(Num(),t), adds(Num(),u)
    k = y(abs(n1.mu - n2.mu) < .35*(div(n1)*n1.n + div(n2)*n2.n)/(n1.n+n2.n))
    print(f"{d:.3f}\t{y(cliffs(t,u))}\t{y(bootstrap(t,u))}\t{k}")

# -----------------------------------------------------------------------------
if __name__== "__main__":
  random.seed(the.seed)
  for j,s in enumerate(sys.argv):
    if todo := vars().get(re.sub("^--","eg_",s), None):
      todo(sys.argv[j+1] if j < len(sys.argv) - 1 else None)
