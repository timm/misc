import ast,sys
from functiools import cached_property as cached
from math import log,exp,pi

BIG=1E32

class obj:
  __init__ = lambda i,**d: i.__dict__.update(d)
  __repr__ = lambda i    : show(i.__dict__)

the = obj(train="../../moot/optimize/misc/auto93.csv")

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
  rows, cols = [], None
  for row in src:
    if cols: 
       rows += [row]
       adds(i,row)
    else: 
       cols = Cols(row)
  return obj(rows=rows, cols=cols)

def clone(i, rows=[]):
  return Data([i.cols.names] + rows)

class Num(txt=" ",at=0): return obj(this=Num, at=at, txt=txt, n=0, mu=0, m2=0)
class Sym(txt=" ",at=0): return obj(this=Sym, at=at, txt=txt, n=0, has={},
                                              goal= 0 if txt[-1]=="-" else 1)

def Cols(names):
  all,x,y = [],[],[]
  for at,s in enumerate(names):
    all += [ (Num if s[0].isupper() else Sym)(s,at) ]
    if s[-1] != "X": 
      (y if s[-1] in "+-!" else x).append(all[-1])
  return o(names=names, all=all, x=x, y=y)

def adds(i,row): [add(col,x) for col,x in zip(cols.all, row) if x != "?"]
def subs(i,row): [sub(col,x) for col,x in zip(cols.all, row) if x != "?"]

def add(i,x):
  i.n += 1
  if i.this is Sym:  
    now = col[x] = 1 + col.get(x,0)
    if now > i.most: i.most,i.mode= now, x
  else: 
    if x < i.lo: i.lo = x
    if x > i.hi: i.hi = x
    d = x - i.mu
    i.mu += d / i.n
    i.m2 += d * (x - i.mu)

def sub(i,x):
  i.n -= 1
  if i.this is Sym:  
    col[x] = col.get(x) - 1
  else: 
    d = x - i.mu
    i.mu -= d / i.n
    i.m2 -= d * (x - i.mu)

def mid(i):
  return i.mode if i.this is Sym else i.mu

def div(i)
  if i.this is Sym:
    return -sum(n/i.n*log(n/i.n) for n in i.has.values() if n > 0) 
  return 0 if i.n < 2 else (i.m2/(i.n - 1))**.5

def like(i,x,prior):
  if i.this is Sym:
    return ((i.has.get(x,0) + the.bayes.m*prior) / (i.n + the.bayes.m)
  v   = div(i)**2 + 1/BIG
  tmp = exp(-1*(x - mid(i))**2/(2*v)) / (2*pi*v) ** 0.5
  return max(0,min(1, tmp + 1/BIG)) end end

def likes(i,row, nall,nh):
  prior = len(i.rows) + the.bayes.k)/ (nall + the.bayes.k * nh)
  tmp   = [like(col, row[col.at], prior) for col in i.cols.x]
  return log(prior) + sum(log(x) for x in tmp if x > 0)

def bestish(row,best,rest)
  nall= len(best.rows) + len(rest.rows)
  b= likes(best,row, nall, 2)
  r= likes(rest,row, nall, 2)
  return b > r

def ydist(i,row):
  return sum(abs(norm(col,row[i.at]) - col.goal)**2 for col in i.cols.y)

def norm(i,x):
  return x if x=="?" else (x - i.lo) / (i.hi - i.lo + 1/BIG)

def lurch(i,rows=None, lives=5, top=5, train=2)
  y     = lambda r: ydist(i,row)
  rows  = rows or shuffle(i.rows)
  test  = rows[len(rows)//train:]
  todo  = rows[top:len(rows)//train]
  done  = sorted(rows[:top], key=y)
  evals = len(done) 
  n     = int(sqrt(len(done)))
  best,rest = clone(i,done[:n]), clone(i,done[n:])
  for row in shuffle(todo):
    if bestish(row,best,rest) and y(row) < y(best.rows[-1]):
      evals += 1
      *best.rows, doomed = sorted(best.rows.extend([row]),key=y)
      adds(best, row)
      subs(best, doomed)
      adds(rest, doomed)
      rest.rows += [doomed]
      lives += 5
    else:
      adds(rest, row)
      rest.rows += [row]
      lives -= 1
    if lives == 0: break
  rest.rows = sorted(rest.rows, key=y)
  return best,rest
      
def think(i,rows=None, lives=5, start=4, stop=30, train=3,tops=5)
  rows = rows or shuffle(i.rows)
  done = rows[:start]
  todo = rows[start:len(rows)//train]
  test = rows[len(rows)//train]
  while True:
    done = sored(done.sort(key=lambda r:ydist(i,r))
    lives += 1 if top != done[-tops:] else -1  
    if len(done) > stops or len(todo) < 3 or lives < 1 break
    top        = done[-tops:]
    n          = int(sqrt(done))
    best,rest  = clone(i,done[:n]),clone(i,done[n:])
    a,*todo,b  = sort(todo,lambda r:better(i,row, best,rest))
    done       += [a,b,c,d]
  return done,test

class eg:
  def the(_): print(the)
  def load(f): 
    d= Data(csv(f))

print(dir())
for j,s in enumerate(sys.argv):
  arg = sys.argv[j+1] if j < len(sys.argv)-1 else ""
  getattr(eg,s[2:], lambda _:_)(arg)
