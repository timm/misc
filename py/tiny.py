import re,ast,sys
from math import log,exp,pi

BIG=1E32

class Obj:
  __init__ = lambda i,**d: i.__dict__.update(d)
  __repr__ = lambda i    : show(i.__dict__)

the = Obj(k=1, m=2,
          train="../../moot/optimize/misc/auto93.csv",
          top=5)

num  = int | float
atom = num | bool | str # | "?"
row  = list[atom]
rows = list[row]
Sym,Num,Data,Cols,Col = Obj,Obj,Obj,Obj,Obj
Col  = Num| Sym

def show(d):
  return '('+' '.join(f":{k} {v}" for k,v in d.items())+')'

def coerce(s):
  try: return ast.literal_eval(s)
  except: return s.strip()

def csv(f):
  with open(f, "r") as file:
    for line in file.readlines():
      yield [coerce(s) for s in line.split(",")]

def Data(src):
  i = Obj(rows=[], cols = None)
  for row in src:
    if    i.cols: addData(i,row)
    else: i.cols = Cols(row)
  return i

def clone(i, rows=[]):
  return Data([i.cols.names] + rows)

def Num(txt=" ",at=0): 
  return Obj(this=Num, at=at, txt=txt, n=0, mu=0, m2=0, lo=BIG, hi= -BIG)

def Sym(txt=" ",at=0): 
  return Obj(this=Sym, at=at, txt=txt, n=0, has={}, 
             most=0, mode=None, goal= 0 if txt[-1]=="-" else 1)

def Cols(names):
  all,x,y = [],[],[]
  for at,s in enumerate(names):
    all += [ (Num if s[0].isupper() else Sym)(s,at) ]
    if s[-1] != "X": 
      (y if s[-1] in "+-!" else x).append(all[-1])
  return Obj(names=names, all=all, x=x, y=y)

def addData(i:Data,row):
  i.rows += [row]
  return addCols(i, row)

def addCols(i:Cols,row):
  [add(col,x) for col,x in zip(i.cols.all, row) if x != "?"]
  return row

def subCols(i:Cols,row):
  [sub(col,x) for col,x in zip(i.cols.all, row) if x != "?"]
  return row

def add(i: Col,x):
  i.n += 1
  if i.this is Sym:
    now = i.has[x] = 1 + i.has.get(x,0)
    if now > i.most: i.most,i.mode= now, x
  else:
    if x < i.lo: i.lo = x
    if x > i.hi: i.hi = x
    d = x - i.mu
    i.mu += d / i.n
    i.m2 += d * (x - i.mu)

def sub(i:Col,x):
  i.n -= 1
  if i.this is Sym:  
    col[x] = col.get(x) - 1
  else: 
    d = x - i.mu
    i.mu -= d / i.n
    i.m2 -= d * (x - i.mu)

def mid(i):
  return i.mode if i.this is Sym else i.mu

def div(i):
  if i.this is Sym: return -sum(n/i.n*log(n/i.n) for n in i.has.values() if n > 0) 
  else            : return 0 if i.n < 2 else (i.m2/(i.n - 1))**.5

def like(i,x,prior):
  if i.this is Sym:
    return (i.has.get(x,0) + the.bayes.m*prior) / (i.n + the.bayes.m)
  else:
    v   = div(i)**2 + 1/BIG
    tmp = exp(-1*(x - mid(i))**2/(2*v)) / (2*pi*v) ** 0.5
    return max(0,min(1, tmp + 1/BIG)) 

def likes(i,row, nall,nh):
  prior = (len(i.rows) + the.bayes.k) / (nall + the.bayes.k * nh)
  tmp   = [like(col, row[col.at], prior) for col in i.cols.x]
  return log(prior) + sum(log(x) for x in tmp if x > 0)

def bestish(row,best,rest):
  nall= len(best.rows) + len(rest.rows)
  b= likes(best,row, nall, 2)
  r= likes(rest,row, nall, 2)
  return b - r

def ydist(i,row):
  return sum(abs(norm(col,row[i.at]) - col.goal)**2 for col in i.cols.y)

def norm(i,x):
  return x if x=="?" else (x - i.lo) / (i.hi - i.lo + 1/BIG)

def lurch(i):
  done        = clone(i, i.rows[:the.top])
  maybe,yes,n = 0,0,int(sqrt(the.done))
  y           = lambda row: ydist(done,row)
  done.rows.sort(key=y)
  best,rest = clone(done, done.rows[:n]), clone(done, done.rows[n:])
  for row in i.rows[the.top:]:
    adds(done, row)
    if bestish(row,best,rest) > 0:
      maybe += 1
      if y(row) < y(best.rows[-1]):
        lives = 5
        yes += 1
        adds(best, row)
        tmp = sorted(best.rows, key=y)
        n = int(sqrt(len(done.rows)))
        [adds(rest, sub(best, doomed)) for doomed in tmp[n:]]
        best.rows = tmp[:n]
        continue
    lives -= 1
    adds(rest, row)
    if lives == 0: break
  best.rows.sort(key=y)
  rest.rows.sort(key=br)
  return best,rest

def eg_the(_): print(the)
def eg_load(f): d= Data(csv(f)); print(d.cols.x)

for j,s in enumerate(sys.argv):
  arg = sys.argv[j+1] if j < len(sys.argv)-1 else ""
  vars().get(re.sub("^--","eg_",s), lambda _:_)(arg)
