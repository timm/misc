#!/usr/bin/env python3 -B
"""
ezr.py: tiny active learning, multi objective.
(c) 2025 Tim Menzies, <timm@ieee.org>. MIT license

 -h                  show help
 -A  Assume=4        on init, how many initial guesses?
 -B  Build=24        when growing theory, how many labels?
 -C  Check=5         when testing, how many checks?
 -D  Delta=smed     required effect size test for cliff's delta
 -F  Few=512         just explore a Few rows
 -a  acq=xploit      acquisition: xploit | xplor | adapt
 -g  guess=0.5       |hot| is |lit|**guess
 -K  Ks=0.95         confidence for Kolmogorov–Smirnov test
 -k  k=1             Bayes hack for rare classes
 -m  m=2             Bayes hack for rare attributes
 -p  p=2             distance calculation coefficient
 -s  seed=1234567891 random number seed
 -f  file=../../../moot/optimize/misc/auto93.csv
                     path to CSV file
"""
from types import SimpleNamespace as o 
from random import choices as some
import random, math, sys, re

def atom(s):
  for fn in [int,float]:
    try: return fn(s)
    except: pass
  s = s.strip()
  return {'True':True, 'False':False}.get(s,s)

the = o(**{k:atom(v) for k,v in re.findall(r"(\w+)=(\S+)",__doc__)})
big = 1E32 

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
  for c,col in enumerate(data.cols.all): add(col,row[c],inc)
  return row

def add(col, v, inc=1):
  if v != "?":
    if type(col) is Sym: col[v] = inc + col.get(v,0)
    else:
      col.n += inc
      col.lo, col.hi = min(v, col.lo), max(v, col.hi)
      if inc < 0 and col.n < 2:
        col.sd = col.m2 = col.mu = col.n = 0
      else:
        d       = v - col.mu
        col.mu += inc * (d / col.n)
        col.m2 += inc * (d * (v - col.mu))
        col.sd  = 0 if col.n < 2 else (max(0,col.m2)/(col.n-1))**.5

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
  print(f'err={err1:.3f}')
  return (out if (n==1 or abs(err - err1) <= 0.01) else
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
  cf = Confuse()
  data = Data(csv(file))
  wait,d,k = 5,{},data.cols.klass
  for n,row in enumerate(data.rows):
    want = row[k]
    d[want] = d.get(want) or clone(data)
    adds(d[want], row)
    if n > wait:
      got = max(d,key=lambda k:likes(d[k],row,n-wait,len(d)))
      confuse(cf,want,got)
  [print(o(**d.__dict__)) for _,d in confused(cf).items()]

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

def Confuse(): return o(data={}, total=0)

def confuse(cf, want, got):
  for x in (want, got):
    if x not in cf.data: 
      cf.data[x] = o(label=x,tp=0,fp=0,fn=0,tn=cf.total)
  for c in cf.data.values():
    if c.label==want: c.tp += (got==want);    c.fp += (got != want)
    else            : c.fn += (got==c.label); c.tn += (got != c.label)
  cf.total += 1
  return got

def confused(cf, summary=False):
  def finalize(c):
    p = lambda y, z: int(100 * y / (z or 1e-32))
    c.pd   = p(c.tp, c.tp + c.fp)
    c.pf   = p(c.fn, c.fn + c.tn)
    c.prec = p(c.tp, c.tp + c.fn)
    c.acc  = p(c.tp + c.tn, c.tp + c.fp + c.fn + c.tn)
    return c

  if summary:
    out = o(label="-", tp=0, fp=0, fn=0, tn=0)
    for c in cf.data.values():
      w = (c.tp + c.fp) / cf.total
      out.tp += c.tp * w
      out.fp += c.fp * w
      out.fn += c.fn * w
      out.tn += c.tn * w
    return finalize(out)
  else:
    return {k: finalize(v) for k, v in cf.data.items()}

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
  ks     = {0.1:1.22, 0.05:1.36, 0.01:1.63}[round(1 - ks,2)]
  cliffs = {'small':0.11, 'smed':0.195, 'medium':0.28, 'large':0.43}[cliffs]
  return _cliffs() <= cliffs and _ks() <= ks * ((n + m)/(n * m))**0.5

def scottknott(rxs, same=ks_cliffs, eps=None):
  eps = eps or .2 * has([x for vs in rxs.values() for x in vs]).sd
  items = [(sum(vs), k, vs, len(vs)) for k, vs in rxs.items()]
  return _sk(sorted(items), same, {}, eps, rank=1)[1]

def _sk(groups, same, out, eps, rank=1):
  def flat(lst): return [x for _, _, xs, _ in lst for x in xs]
  c = _skcut(groups, eps)
  if c and not same(flat(groups[:c]), flat(groups[c:])):
    return _sk(groups[c:], same, out, eps,
               rank=_sk(groups[:c], same, out, eps, rank)[0])
  for _, k, _, _ in groups: out[k] = rank
  return rank + 1, out

def _skcut(groups, eps):
  sum1 = sum(s for s, _, _, _ in groups)
  n1   = sum(n for _, _, _, n in groups)
  mu   = sum1 / n1
  best = sum0 = n0 = score = 0
  for j, (s, _, _, n) in enumerate(groups[:-1]):
    sum0 += s; n0 += n
    sum1 -= s; n1 -= n
    mu0, mu1 = sum0 / n0, sum1 / n1
    now = n0 * (mu0 - mu)**2 + n1 * (mu1 - mu)**2
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
        yield [atom(val.strip()) for val in line.split(",")]

def has(src, col=None):
  for x in src:
    col = col or (Num if type(x) in [int,float] else Sym)()
    add(col, x)
  return col

#  _        _.  ._ _   ._   |   _    _
# (/_  ><  (_|  | | |  |_)  |  (/_  _> 
#                      |               

def eg__all():
  for k,fn in globals().items():
    if k.startswith('eg__') and k != 'eg__all':
      print("\n----["+k+"]"+'-'*40)
      random.seed(the.seed)
      fn()

def eg_h(): print(__doc__)
def eg__the(): print(the)
def eg__csv(): [print(t) for t in csv(the.file)]
def eg__sym(): print(has("aaaabbc"))
def eg__Sym(): s = has("aaaabbc"); assert 0.44 == round(like(s,"a"),2)
def eg__num(): print(has(random.gauss(10,2) for _ in range(1000)))
def eg__Num() : 
  n = has(random.gauss(10,2) for _ in range(1000))
  assert 0.13 == round(like(n,10.5),2)

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

def eg__confuse():
  """
  a b c <- got
  ------. want
  5 1   | a
    2 1 | b
      3 | c
  """
  cf = Confuse()   
  for want,got,n in [
      ("a","a",5),("a","b",1),("b","b",2),("b","c",1),("c","c",3)]:
    for _ in range(n): confuse(cf, want, got)
  xpect = {"a": dict(pd=83,  acc=91, pf=0,  prec=100),
           "b": dict(pd=66,  acc=83, pf=11, prec=66),
           "c": dict(pd=100, acc=91, pf=11, prec=75) }
  for k, y in confused(cf).items():
    got = dict(pd=y.pd, acc=y.acc, pf=y.pf, prec=y.prec)
    assert got == xpect[k]
    print(k, o(**got))

def eg__stats():
   b4 = [random.gauss(1,1)+ random.gauss(10,1)**0.5 for _ in range(59)]
   d, out = 0,[]
   while d < 0.7:
     now = [x+d*random.random() for x in b4]
     out += [f"{d:.2f}" + ("y" if ks_cliffs(b4,now) else "n")]
     d += 0.05
   print(', '.join(out))

def eg__sk():
  n=500
  rxs=dict(asIs = [random.gauss(10,1) for _ in range(n)],
          copy1 = [random.gauss(20,1) for _ in range(n)],
          now1  = [random.gauss(20,1) for _ in range(n)],
          copy2 = [random.gauss(40,1) for _ in range(n)],
          now2  = [random.gauss(40,1) for _ in range(n)])
  print(scottknott(rxs))
  #[print(o(rank=num.rank, mu=num.mu)) for num in scottKnott(rxs).values()]

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
