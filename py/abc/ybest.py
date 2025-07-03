# vim: set ts=4 sw=4 sts=4 et:
from types import SimpleNamespace as o
from random import choices as some
import random, sys

Atom = [bool|float|int|str]
Row=list[Atom]
Col="Num" | "Sym"

big=1E32
the = o(bins=12, 
        Build=30,   
        cohen=0.35,
        Few=64, 
        file="../../../moot/optimize/misc/auto93.csv",
        k=2,
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
  for c,col in enumerate(data.cols.all) : row[c]= add(col,row[c],inc)
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
  tmp = [like(col,row[c.at],prior) 
         for c in data.cols.y if y not in row[c.at] != "?"]
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
def acquires(data, rows):
  best, rest, labelled = clone(data), clone(data), clone(data)
  random.shuffle(rows)
  budget = the.Build
  for n,row in enumerate(rows): 
    if budget <= 0: break
    elif len(best.rows) < the.Budget**the.guess:
      budget -= 1
      adds(best, adds(labelled, row))
    elif likes(rest,row,2,n) > likes(best,row,2,n):
      adds(rest,row)
    else:
      budget -= 1
      adds(best, adds(labelled, row))
      best.rows.sort(key = lambda row:ydist(labelled, row))
      adds(rest, adds(best, best.rows.pop(0), -1))
  return best, rest, rows[n:]

#-------------------------------------------------------------------
def best(data,rows):
  Y = lambda r: ydist(data,r)
  return round(Y(sorted( some(data.rows,k=the.Build), key=Y)[0]),2)

def rank(rxs, reverse=False):
  def bag(k,v): 
    v=sorted(v); return o(key=k, vals=v, mu=v[len(v)//2], sd=sd(v), rank=0)

  d,l,stdev = {}, [], sd(sorted(z for l in rxs.value() for z in l))
  for b in sorted([bag(k, v) for k, v in rxs.items()],
                    key=lambda z: z.mu, reverse=reverse):
    k = b.key
    if l and abs(l[-1].mu - b.mu) <= stdev * the.cohen:
      b = bag('_', l.pop().vals + b.vals)
    b.key = k
    b.rank = len(l)
    l += [b]
    d[k] = b
  return [(k, d[k].rank, d[k].mu, d[k].sd) for k in rxs]

def sd(x):
  lo,mu,hi=(x[0],x[len(x)//2],x[-1]) if type(x) is list else (x.lo,x.mu,x.hi)
  return ((lo**2 + hi**2 + mu**2 - lo*hi - lo*mu - hi*mu) / 18) ** 0.5

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
