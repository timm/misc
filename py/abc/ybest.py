# vim: set ts=4 sw=4 sts=4 et:
from types import SimpleNamespace as o
from random import choices as some
import random, sys

big=1E32
the = o(bins=12, 
        Build=30,   
        Delta="small",
        Few=64, 
        file="../../../moot/optimize/misc/auto93.csv",
        k=2,
        Ks=0.95,
        m=1,
        p=2,
        seed=1234567891)

#--------------------------------------------------------------------
Sym=dict
Num=lambda: o(lo=big, mu=0, m2=0, sd=0, n=0, hi=-big)

def Data(src):
  def _cols(names):
    i = o(names=names, all=[], x={}, y={})
    for c,s in enumerate(names):
      i.all += [Num() if s[0].isupper() else Sym()]
      if s[-1] in "!-+": i.y[c] = s[-1]!="-"
    return i

  src = iter(src)
  data = o(rows=[], cols= _cols(next(src)))
  [adds(data,r) for r in src]
  return data 

def clone(data,rows=[]): return Data([data.cols.names] + rows)

def adds(data, row, inc=1, zap=False):
  if inc>0: data.rows += [row]
  elif zap: data.rows.remove(row)
  for c,col in enumerate(data.cols.all) : row[c] = add(col,row[c],inc)
  return row

def add(col,v,inc=1):
  if v != "?":
    if type(col) is Sym: col[v] = inc + col.get(v,0)
    else:
      v = float(v)
      col.n += 1
      col.lo, col.hi = min(v, col.lo), max(v, col.hi)
      if inc < 0 and col.n < 2:
        col.sd = col.m2 = col.mu = col.n = 0
      else:
        d       = v - col.mu
        col.mu += inc * (d / col.n)
        col.m2 += inc * (d * (v - col.mu))
        col.sd  = 0 if col.n <= 2 else (max(0,col.m2)/(col.n-1))**.5
  return v

#--------------------------------------------------------------------
def like(col, v, prior=0):
  if type(col) is Sym: 
    return (col.has.get(v,0) + the.m*prior) / (col.n + the.m + 1/big)
  var = 2 * col.sd * col.sd
  z   = (v - col.mu) ** 2 / var
  return min(1, max(0, math.exp(-z) / (2 * math.pi * var) ** 0.5))

def likes(data, row, nall=100, nh=2):
  prior = (len(data.rows) + the.k) / (nall + the.k*nh)
  tmp = [like(col,v,prior) 
         for c in data.cols.x if (v:=row[c.at]) != "?"]
  return sum(math.log(n) for n in tmp + [prior] if n>0)    

#--------------------------------------------------------------------
def norm(col,x): 
  if x=="?" or type(col) is Sym: return x
  return (x - col.lo) / (col.hi - col.lo + 1/big)

def ydist(data,row):
  d = 0
  for c,w in data.cols.y.items(): 
    d += abs(norm(data.cols.all[c], row[c]) - w)**2
  return (d/len(data.cols.y)) ** 0.5

def xdist(data,row1,row2):
  def _dist(col, a,b):
    if a==b=="?": return 1
    if type(col) is Sym: return a != b
    a,b = norm(col,a), norm(col,b)
    a = a if a != "?" else (0 if b>0.5 else 1)
    b = b if b != "?" else (0 if a>0.5 else 1)
    return abs(a-b)
    
  d,n = 0,0
  for c,col in enumerate(data.cols.all):
    if c not in data.cols.y:
      n += 1
      d += _dist(col, row1[c], row2[c])**the.p
  return (d/n) ** (1/the.p)

def kpp(data,rows,k=20, few=100):
  random.shuffle(rows)
  out = [rows[0]]
  while len(out) < k:
    tmp = some(rows,k=few)
    ws = [min(xdist(data,r,c)**2 for c in out) for r in tmp]
    out.append(random.choices(tmp, weights=ws)[0])
  return out

#--------------------------------------------------------------------
def acquires(data, unlabelled, assume=the.Assume, budget=the.Build):
  labelled = clone(data)
  best     = clone(data) # subset of labelled
  rest     = clone(data) # rest = labelled - best
  inits    = max(assume, budget**.5)
  _like    = lambda what,row: likes(what, row, 2, len(labelled.rows))
  _ydist   = lambda row: ydist(labelled, row) # smaller is better

  random.shuffle(unlabelled)
  for n,row in enumerate(unlabelled): 
    if len(labelled.rows) > max(inits,budget): break
    if len(todo.rows) < inits or _like(best,row) > _like(rest,row):
      adds(best, adds(labelled, row))
    if len(best.rows) > budget**.5:
      best.rows.sort(key=_ydist)
      adds(rest, adds(best, best.rows.pop(0), -1))
  return o(labelled   = sorted(labelled.rows, key=_ydist), 
           unlabelled = unlabelled[n:], 
           best       = best, 
           rest       = rest)

#-------------------------------------------------------------------
def best(data,rows):
  Y = lambda r: ydist(data,r)
  return round(Y(sorted( some(data.rows,k=the.Build), key=Y)[0]),2)

def ks_cliffs(x, y, ks=the.Ks, cliffs=the.Delta):
  n, m = len(x), len(y)
  ks     = {0.1:1.22, 0.05:1.36, 0.01:1.63}[1 - ks]
  cliffs = {'small':0.11, 'medium':0.28, 'large':0.43}[cliffs]
  return abs(_cliffs(x, y, n, m)) <= cliffs and \
         _ks(x, y, n, m) <= ks * ((n + m)/(n * m))**0.5

def _cliffs(x, y, n, m):
  gt = sum(i > j for i in x for j in y)
  eq = sum(i == j for i in x for j in y)
  return 2 * (gt + 0.5 * eq) / (n * m) - 1 #cliffs = 2*vda-1

def _ks(x, y, n, m):
  x, y = sorted(x), sorted(y)
  xs = sorted(x + y)
  fx = [sum(i <= v for i in x)/n for v in xs]
  fy = [sum(i <= v for i in y)/m for v in xs]
  return max(abs(a - b) for a, b in zip(fx, fy))

def rank(rxs, reverse=False, same=ks_cliffs):
  def bag(k, v):
    v = sorted(v)
    return o(keys=k, vals=v, mu=v[len(v)//2], sd=sd(v), rank=0)

  bags = sorted([bag([k], v) for k, v in rxs.items()],
                key=lambda z: z.mu, reverse=reverse)
  lst = []
  for b in bags:
    if lst and same(lst[-1].vals, b.vals):
      last = lst.pop()
      b = bag(last.keys + b.keys, last.vals + b.vals)
    b.rank = len(lst) + 1
    lst += [b]
  return {k: b for b in lst for k in b.keys}

def sd(x): #???needed
  if type(x)==list: 
    ten=len(x)//10; return (x[9*ten] - x[ten])/2.56
  else:
    return ((x.lo**2 + x.hi**2 + x.mid**2 
           - x.lo*x.hi - x.lo*x.mid - x.hi*x.mid) / 18) ** 0.5

#-------------------------------------------------------------------
def cli(data):
  for n,arg in enumerate(sys.argv):
    for k in data:
      if arg == "-" + k[0]: 
        data[k] = type(data[k])(sys.argv[n+1])

def csv(file=None):
  with open(file) as f:
    for line in f:
      if (line := line.split("%")[0]):
        yield [val.strip() for val in line.split(",")]

#-------------------------------------------------------------------
def thrash(data) : return best(data, some( data.rows, k=the.Build))
def div12(data)  : return best(data, kpp(data,data.rows, k=the.Build,few=12))
def div24(data)  : return best(data, kpp(data,data.rows, k=the.Build,few=24))
def div50(data)  : return best(data, kpp(data,data.rows, k=the.Build,few=50))
def div100(data) : return best(data, kpp(data,data.rows, k=the.Build,few=100))
def div200(data) : return best(data, kpp(data,data.rows, k=the.Build,few=200))

cli(the.__dict__)
print(the)
random.seed(the.seed)
data= Data(csv(the.file))
for col in data.cols.all:
    if type(col) is not Sym: print(sd(col))

for fn in [thrash]:#,div12,div24,div50,div100,div200]:
    print(sorted(fn(data) for _ in range(the.Build))[::5], fn.__name__)
