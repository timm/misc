# vim: set ts=4 sw=4 sts=4 et:
from types import SimpleNamespace as o
from random import choices as some
import random, math, sys

big = 1E32
the = o(Assume=4,       # budget for initial guesses before modeling
        Build=30,       # budget for sampling for theory building
        Delta="small",  # cliffs delta effect size
        Few=64,         # some subset of rows
        file="../../../moot/optimize/misc/auto93.csv",
        k=1,            # bayes hack for low frequency classes
        Ks=0.95,        # KS test confidence
        m=2,            # bayes hack for low frequency attributes
        p=2,            # distance calculation coeffecient
        seed=1234567891) # random seed

#  _  _|_  ._        _  _|_   _ 
# _>   |_  |   |_|  (_   |_  _> 

Sym = dict
Num = lambda: o(lo=big, mu=0, m2=0, sd=0, n=0, hi=-big, w=1)

def Data(src):
  def _cols(names):
    i = o(names=names, all=[], x={}, y={}, klass=None)
    for c,s in enumerate(names):
      i.all += [Num() if s[0].isupper() else Sym()]
      if s[-1] != "X":
        if s[-1] == "!": i.klass=c
        if s[-1] == "-": i.all[-1].w = 0
        (i.y if s[-1] in "!-a" else i.x)[c] = i.all[-1]
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

def add(col, v, inc=1):
  if v != "?":
    if type(col) is Sym: col[v] = inc + col.get(v,0)
    else:
      v = float(v)
      col.n += inc
      col.lo, col.hi = min(v, col.lo), max(v, col.hi)
      if inc < 0 and col.n < 2:
        col.sd = col.m2 = col.mu = col.n = 0
      else:
        d       = v - col.mu
        col.mu += inc * (d / col.n)
        col.m2 += inc * (d * (v - col.mu))
        col.sd  = 0 if col.n <= 2 else (max(0,col.m2)/(col.n-1))**.5
  return v

#   _|  o   _  _|_   _.  ._    _   _  
#  (_|  |  _>   |_  (_|  | |  (_  (/_ 
                                    
def ydist(data,row):
  d = sum(abs(norm(col,row[c]) - w)**2 
          for c,col in data.cols.y.items()) 
  return (d/len(data.cols.y)) ** 0.5

def xdist(data,row1,row2):
  def _aha(col, a,b):
    if a==b=="?": return 1
    if type(col) is Sym: return a != b
    a,b = norm(col,a), norm(col,b)
    a = a if a != "?" else (0 if b>0.5 else 1)
    b = b if b != "?" else (0 if a>0.5 else 1)
    return abs(a-b)
    
  d = sum(_aha(col, row1[c], row2[c])**the.p 
          for c,col in data.cols.x.items())
  return (d/len(data.cols.x)) ** (1/the.p)

def norm(col,x): 
  if x=="?" or type(col) is Sym: return x
  return (x - col.lo) / (col.hi - col.lo + 1/big)

def kpp(data,rows=None,k=20, few=the.Few):
  rows = rows or data.rows
  random.shuffle(rows)
  out = [rows[0]]
  while len(out) < k:
    tmp = some(rows,k=few)
    ws = [min(xdist(data,r,c)**2 for c in out) for r in tmp]
    out.append(random.choices(tmp, weights=ws)[0])
  return out

def kmeans(data,rows=None, n=10,out=None,err=1,**k):
  rows = rows or data.rows
  centroids = [mids(d) for d in out] if out else kpp(data,rows,**k)
  d,err1 = {},0
  for row in rows:
    c = min(centroids, key=lambda c: xdist(data,c,row))
    err1 += xdist(data,c,row) / len(rows)
    d[id(c)] = d.get(id(c)) or clone(data)
    adds(d[id(c)],row)
  return (out if n==1 or abs(err - err1) < 0.01 else
          kmeans(data, rows, n-1, d.values(), err=err1,**k))

def mids(data):
  return [(max(c, key=c.get) if type(c) is dict else c.mu)
          for c in data.cols.all]

#  |  o  |    _  
#  |  |  |<  (/_ 
               
def like(col, v,prior=0):
  if type(col) is Sym: 
    return (col.get(v,0)+the.m*prior)/(sum(col.values())+the.m+1/big)
  var = 2 * col.sd * col.sd
  z   = (v - col.mu) ** 2 / var
  return min(1, max(0, math.exp(-z) / (2 * math.pi * var) ** 0.5))

def likes(data, row, nall=100, nh=2):
  prior = (len(data.rows) + the.k) / (nall + the.k*nh)
  tmp = [like(col,v,prior) 
         for c,col in data.cols.x.items() if (v:=row[c]) != "?"]
  return sum(math.log(n) for n in tmp + [prior] if n>0)    

def nbc(file, wait=5):
  data = Data(csv(file))
  wait,acc,d,k = 5,0,{},data.cols.klass
  for n,row in enumerate(data.rows):
    want = row[k]
    d[want] = d.get(want) or clone(data)
    adds(d[want], row)
    if n > wait:
      got = max(d,key=lambda k:likes(d[k],row,n-wait,len(d)))
      acc += want == got
  print(acc/(n-wait))

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
           best = best, rest = rest)

#   _  _|_   _.  _|_   _ 
#  _>   |_  (_|   |_  _> 

def confusions():
  def _add1(c, want, got, x):
    if x == want: c.tp += (got == want); c.fp += (got != want)
    else:         c.fn += (got == x);    c.tn += (got != x)

  def _final(c):
    p = lambda y,z: int(100 * y / (z or 1e-32))
    c.pd   = p(c.tp, c.tp + c.fp)
    c.pf   = p(c.fn, c.fn + c.tn)
    c.prec = p(c.tp, c.tp + c.fn)
    c.acc  = p(c.tp + c.tn, c.tp + c.fp + c.fn + c.tn)
    return c

  def add(want, got):
    for lbl in (want, got):
      if lbl not in data:
        data[lbl] = o(label=lbl, tn=total[0], tp=0, fp=0, fn=0)
    [_add1(c, want, got, c.label) for c in data.values()]
    total[0] += 1

  def summary():
    out = o(label="-", tn=0, tp=0, fp=0, fn=0)
    for c in data.values():
      w = (c.tp + c.fp) / total[0]
      out.tp += c.tp * w
      out.fp += c.fp * w
      out.fn += c.fn * w
      out.tn += c.tn * w
    return _final(out)

  data, total = {}, [0]
  return o(add=add, summary=summary,
           final=lambda: ([_final(c) for c in data.values()], data)[1])

def ks_cliffs(x, y, ks=the.Ks, cliffs=the.Delta):
  def _cliffs():
    gt = sum(i > j for i in x for j in y)
    lt = sum(i < j for i in x for j in y)
    return abs(gt - lt) / (n * m)

  def _ks():
    xs = sorted(x + y)
    fx = [sum(i <= v for i in x)/n for v in xs]
    fy = [sum(i <= v for i in y)/m for v in xs]
    return max(abs(a - b) for a, b in zip(fx, fy))

  x, y = sorted(x), sorted(y)
  n, m = len(x), len(y)
  ks     = {0.1:1.22, 0.05:1.36, 0.01:1.63}[1 - ks]
  cliffs = {'small':0.11, 'medium':0.28, 'large':0.43}[cliffs]
  return _cliffs() <= cliffs and _ks() <= ks * ((n + m)/(n * m))**0.5

def scottknott(rxs, same=ks_cliffs, eps=0):
  items = sorted((x, k) for k, vs in rxs.items() for x in vs)
  return _sk(items, same, {}, eps, 1)[1]

def _sk(xy, same, out, eps, rank):
  c = _skcut(xy, eps)
  if best and not same([x for x,_ in xy[:c]], [x for x,_ in xy[c:]]):
    return _sk(xy[c:],same,out,eps, _sk(xy[:x],same,out,eps,rank)[0])
  for _, k in xy: out[k] = rank
  return rank + 1, out

def _skcut(xy, eps):
  mu = sum(x for x, _ in xy) / len(xy)
  n0 = sum0 = score = 0
  n1, sum1 = len(xy), sum(x for x,_ in xy)
  best = 0
  for j,(x,_) in enumerate(xy[:-1]):
    n0 += 1; sum0 += x
    n1 -= 1; sum1 -= x
    mu0, mu1 = sum0 / n0, sum1 / n1
    now = n0*(mu0 - mu)**2 + n1*(mu1 - mu)**2
    if abs(mu0 - mu1) > eps and now > score: 
       score, best = now, j + 1
  return best

#   _                                     
# _|_       ._    _  _|_  o   _   ._    _ 
#  |   |_|  | |  (_   |_  |  (_)  | |  _> 
                                                        
def cli(d):
  for n,arg in enumerate(sys.argv):
    for k in d:
      if arg == "-"+k[0]: d[k] = type(d[k])(sys.argv[n+1])

def csv(file=None):
  with open(file) as f:
    for line in f:
      if (line := line.split("%")[0]):
        yield [val.strip() for val in line.split(",")]

def has(src, col=None):
  for x in src:
    col = col or (Num if type(x) in [int,float] else Sym)()
    add(col, x)
  return col

# _|_   _    _  _|_   _ 
#  |_  (/_  _>   |_  _> 
                       
def eg__the(): print(the)
def eg__csv(): [print(t) for t in csv(the.file)]
def eg__sym(): print(has("aaaabbc"))
def eg__Sym(): s = has("aaaabbc"); assert 0.44 == round(like(s,"a"),2)
def eg__num(): print(has(random.gauss(10,2) for _ in range(1000)))
def eg__Num() : 
  n = has(random.gauss(10,2) for _ in range(1000))
  assert 0.13== round(like(n,10.5),2) == round(like(n,9.5,2))

def eg__data(): [print(col) for col in Data(csv(the.file)).cols.all]

def eg__inc():
  d1 = Data(csv(the.file))
  d2 = clone(d1)
  x  = d2.cols.x[1]
  for row in d1.rows:
    adds(d2,row)
    if len(d2.rows)==100: mu1,sd1 = x.mu,x.sd 
  for row in d1.rows[::-1]:
    if len(d2.rows)==100: mu2,sd2 = x.mu,x.sd
    adds(d2,row, inc=-1, zap=True)
  assert abs(mu2 - mu1) < 1.01 and abs(sd2 - sd1) < 1.01

def eg__bayes():
  data = Data(csv(the.file))
  assert all(-30 <= likes(data,t) <= 0 for t in data.rows)
  print(sorted([round(likes(data,t),2) for t in data.rows])[::20])

def eg__diabetes(): nbc("../../../moot/classify/diabetes.csv")
def eg__soybean():  nbc("../../../moot/classify/soybean.csv")

def eg__irisKpp(): 
  [print(r) for r in kpp(Data(csv("../../../moot/classify/iris.csv")),k=10)]

def eg__irisK(): 
  for data in kmeans(Data(csv("../../../moot/classify/iris.csv")),k=10):
    print(mids(data)) 

if __name__ == "__main__": 
  cli(the.__dict__)
  for n,arg in enumerate(sys.argv):
    if (fn := globals().get(f"eg{arg.replace('-', '_')}")):
      random.seed(the.seed); fn()
