from fileinput import input as finput
from types import SimpleNamespace as o
from random import choices as some
import random, sys

the = o(bins=12, 
        Build=30, 
        cohen=0.35,
        Few=64, 
        file="../../../moot/optimize/misc/auto93.csv",
        p=2,
        seed=1234567891)

big=1E32
Sym=dict
Num=lambda: o(lo=big, mu=0, hi=-big)

def Data(src):
  def _cols(names):
    i = o(names=names, all=[], y={})
    for c,s in enumerate(names):
      i.all += [Num() if s[0].isupper() else Sym()]
      if s[-1] in "!-+": i.y[c] = s[-1]!="-"
    return i

  src = iter(src)
  data = o(rows=[], cols= _cols(next(src)))
  [add(data,r) for r in src]
  return data 

def add(data, row):
  def _add(c,col,v):
    if v != "?":
      if type(col) is Sym: col[v] = col.get(v,0) + 1
      else:
        v = row[c] = float(v)
        col.lo  = min(v, col.lo)
        col.hi  = max(v, col.hi)
        col.mu += (v - col.mu)/len(data.rows)

  data.rows += [row]
  for c,col in enumerate(data.cols.all) : _add(c,col,row[c])

def norm(col,x): 
  if x=="?" or type(col) is Sym: return x
  return (x - col.lo) / (col.hi - col.lo + 1/big)

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

def ydist(data,row):
  d = 0
  for c,w in data.cols.y.items(): 
    d += abs(norm(data.cols.all[c], row[c]) - w)**2
  return (d/len(data.cols.y)) ** 0.5

def kpp(data,rows,k=20, few=100):
  random.shuffle(rows)
  out = [rows[0]]
  while len(out) < k:
    tmp = some(rows,k=few)
    ws = [min(xdist(data,r,c)**2 for c in out) for r in tmp]
    out.append(random.choices(tmp, weights=ws)[0])
  return out

def sd(lo,mu,hi):
  return ((lo**2 + hi**2 + mu**2 - lo*hi - lo*mu - hi*mu) / 18) ** 0.5

def thrash(data)     : return best(data, some( data.rows, k=the.Build))
def diverse12(data)  : return best(data, kpp(data,data.rows, k=the.Build,few=12))
def diverse24(data)  : return best(data, kpp(data,data.rows, k=the.Build,few=24))
def diverse50(data)  : return best(data, kpp(data,data.rows, k=the.Build,few=50))
def diverse100(data) : return best(data, kpp(data,data.rows, k=the.Build,few=100))
def diverse200(data) : return best(data, kpp(data,data.rows, k=the.Build,few=200))

def best(data,rows):
  Y = lambda r: ydist(data,r)
  return round(Y(sorted( some(data.rows,k=the.Build), key=Y)[0]),2)

def cli(data):
  for n,arg in enumerate(sys.argv):
    for k in data:
      if arg == "-" + k[0]: 
        data[k] = type(data[k])(sys.argv[n+1])

def csv(files=None):
  for line in finput(files=files):
    if line: yield [s.strip() for s in line.split(",")]

def rank(rxs, reverse=False):
  def mid(l): return l[len(l)//2]
  def bag(k,v):
    return o(key=k, vals=v, mu=mid(v), sd=sd(v[0], mid(v), v[-1]), rank=0)
  all   = sorted([x for l in rxs.value() for x in l])
  cohen = the.cohen * sd(all[0], mid(all),all[-1])
  out,tmp= {},[]
  for now in sorted([bag(k,sorted(v_) for k,v in rxs.items()], 
                    reverse=reverse, 
                    key=lambda z: z.mu):
    if tmp and abs(tmp[-1].mu - now.mu) <= cohen:
      tmp[-1] = bag(now.key, sorted(now.vals+tmp[-1].vals))
    else:
      tmp += [now]
    now.rank = chr(96+len(tmo))
    out[now.key] = now
  return [(k, out[k].rank, out[k].mu, out[k].sd) for k in rxs]

cli(the.__dict__)
print(the)
random.seed(the.seed)
data= Data(csv(the.file))
for col in data.cols.all:
    if type(col) is not Sym: print(sd(col))

for fn in [thrash]:#,diverse12,diverse24,diverse50,diverse100,diverse200]:
    print(sorted(fn(data) for _ in range(the.Build))[::5], fn.__name__)
