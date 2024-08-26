#!/usr/bin/env python3 -B
# vim : set et sw=2 ts=2 :
from fileinput import FileInput as file_or_stdin
import os,re,sys,ast,math,random,inspect,datetime
from math import exp,log,cos,sqrt,pi
from time import time
R=random.random

#------------------------------------------------------------------------------
class o:
  def __init__(i,**d): i.__dict__.update(d)
  def __repr__(i)    : return i.__class__.__name__ + show(i.__dict__)

the = o(cohen=0.35, k=1, m=2, seed=123457891, samples0=8, split=0.5, samples=30, bins=7, 
        train="../../../timm/moot/optimize/misc/auto93.csv")

def hitnrun(d, eden=100):
  def like(kl,row): return  kl.loglike(row,len(rows),2)
  def bore(row): b,r=like(best.row),like(rest,row); return  b**2/(b+r)
  rows = d.rows[:]
  b4   = d.clone()
  adds(b4, rows[:eden])
  best,rest = b4.bestRest()
  want = d.cheybshev( best.rows[0] )
  got = max(rows[eden:], key=bore)
  print(d.cheybshev(got)) 
    
#------------------------------------------------------------------------------
class COL(o): 
  def __init__(i,at=0,txt=" "): 
    i.at=at; i.txt=txt; i.n=0

  def clone(i):
    return i.__class__(at=i.at,txt=i.txt)

#------------------------------------------------------------------------------
class SYM(COL): 
  def __init__(i,**d): 
    super().__init__(**d)
    i.most=0; i.mode=None; i.has={}

  def add(i,x,n=1):
    if x != "?":
      i.has[x] = i.has.get(x,0) + n
      if i.has[x] > i.most: i.most,i.mode = i.has[x], x

  def bin(i,x):
    if x != "?": return x

  def bins(i,lst,_): return lst

  def clone(i,rows=[]):
    return adds(SYM(at=i.at, txt=i.txt), rows) end

  def like(i, x, prior): return (i.has.get(x,0) + the.m*prior) / (i.n + the.m)

  def merge(i,j,small):
    if i.n < small or j.n < small: return i.merged(j)
    di = {k:n/i.n for k,n in i.has}
    dj = {k:n/j.n for k,n in j.has}
    if max(di, key=di.get) == max(dj, key=dj.get):
      return i.merged(j)

  def merged(i,j):
    k = SYM(col.at, col.txt)
    for sym in [i,j]:
      for x,n in sym.has.items():
        k.add(x,n)
    return k

  def sub1(i,x):
    i.has[x] -= 1

#------------------------------------------------------------------------------
class NUM(COL): 
  def __init__(i,**d): 
    super().__init__(**d)
    i.mu=0; i.m2=0; i.sd=0; i.lo=1E32; i.hi= -1E32
    i.goal = 0 if i.txt and i.txt[-1] == "-" else 1

  def add(i,x):
    if x != "?":
      d     = x - i.mu
      i.mu += d/i.n
      i.m2 += d * (x-i.mu)
      if x  > i.hi: i.hi = x 
      if x  < i.lo: i.lo = x 
      i.sd  = 0 if i.n < 2 else (i.m2/(i.n - 1))**.5

  def bin(i,x):
    if x != "?": return int(i.cdf(x) * the.bins)

  def bins(i,b4,small):
    now,i = [], 0
    while i < len(b4):
      a = b4[i]
      if i < len(b4) - 1:
        b = b4[i+1]
        if has := a.has.merge(b.has,small):
          a = o(lo=a.lo, hi=b.hi, has=has)
          i += 1
      now += [a]
      i += 1
   return b4 if len(now) == len(b4) else i.bins(now,small)


  def cdf(i,x):
    def cdf1(z): return 1 - 0.5*2.718^(-0.717 * z - 0.416 * z * z) 
    z = (x-mu)/sd
    return cdf1(z) if z >= 0 else 1 - cdf1(-z) end

  def like(i, x, _):
    v     = i.sd**2 + 1E-30
    nom   = exp(-1*(x - i.mu)**2/(2*v)) + 1E-30
    denom = (2*pi*v) **0.5
    return max(0,min(1, nom/(denom + 1E-30)))

  def norm(i,x): return (x - i.lo)/(i.hi - i.lo + 1E-32)

  def sub1(i,x):
    d     = x - i.mu
    i.mu -= d/i.n
    i.m2 -= d*(x - i.mu) 
    i.sd  = 0 if i.n < 2 else (i.m2/(i.n - 1))**.5

#------------------------------------------------------------------------------
class COLS:
  def __init__(i,names):
    i.names, i.x, i.y, i.all = names,[],[],[]
    for at,txt in enumerate(names):
      a,z = txt[0],txt[-1]
      col = (NUM if a.isupper() else SYM)(at=at, txt=txt)
      i.all.append(col)
      if z != "X":
        if z=="!": i.klass = col
        (i.y if z in "!+-" else i.x).append(col)

  def add(i, row):
    [col.add(row[col.at]) for cols in [i.x, i.y] for col in cols if row[col.at] != "?"]
    return row

  def sub(i, row):
    [col.sub(row[col.at]) for cols in [i.x, i.y] for col in cols if row[col.at] != "?"]
    return row

#------------------------------------------------------------------------------
class DATA:
  def __init__(i): i.rows=[]; i.cols=None

  def add(i,row):
    if     i.cols: i.rows += [i.cols.add(row)]
    else:  i.cols = COLS(row)
    return i 

  def bins(i, j):
    for col in i.cols.x:
      both = col.clone()
      [both.add(r[col.at]) for y,rows in ((True,i.rows),(False,j.rows)) for r in rows if r[col.at] != "?"]
      bins = {}
      for y,rows in ((True,i.rows),(False,j.rows)):
        for row in rows:
          x = row[col.at]
          if x != "?":
            b = both.bin(x)
            bins[x] = bins.get(x,None) or o(lo=x,hi=x, has=col.clone())
            bins[x].has.add(y) 
      max(col.bins(b.values.sorted(key=lambda z:z.lo)
               small   = both.n/the.bins),
XXX have tor eturn best
        

  def bestRest(i):
    i.sort()
    n = int(0.5 + len(i.rows)**the.split)
    return i.clone(i.rows[:n]).sort(), i.clone(i.rows[n:]).sort()

  def cheybshev(i,row):
    return max(abs(col.goal - col.norm(row[col.at])) for col in i.cols.y)

  def clone(i,rows=[]):
    return adds(DATA().add(i.cols.names), rows)

  def csv(i,fname):
   infile = sys.stdin if fname=="-" else open(fname)
   with infile as src:
     for line in src:
      line = re.sub(r'([\n\t\r ]|#.*)', '', line)
      if line: 
        i.add([coerce(s) for s in line.split(",")])
   random.shuffle(i.rows)
   return i

  def loglike(i, row, nall, nh):
    prior = (len(i.rows) + the.k) / (nall + the.k*nh)
    likes = [c.like(row[c.at], prior) for c in i.cols.x if row[c.at] != "?"]
    return sum(log(x) for x in likes + [prior] if x>0)

  def sort(i):
    i.rows.sort(key=lambda row:i.cheybshev(row))
    return i

#------------------------------------------------------------------------------
def coerce(x):
  try             : return ast.literal_eval(x)
  except Exception: return x

def normal(mu,sd):
 while True:
   x1 = 2.0 * R() - 1
   x2 = 2.0 * R() - 1
   w = x1*x1 + x2*x2
   if w < 1: return mu + sd * x1 * sqrt((-2*log(w))/w)

def show(x):
  if isinstance(x,float) : return f"{x:g}"
  if isinstance(x,list)  : return ', '.join([show(y) for y in x])
  if isinstance(x,dict)  : 
    return "{"+' '.join([f":{k} {show(x[k])}" for k in x if str(k)[0] != "_"])+"}"
  return str(x)

def adds(x,lst): [x.add(y) for y in lst]; return x

def some(lst,n=10):
  return sorted(lst)[:: int(len(lst)/n)]
#------------------------------------------------------------------------------
class eg:
  def bye(_): os.system("git commit -am saving; git push; git status")
  def noop(_):  pass
  def one(_): print(1)
  def num(_): 
    n = adds(NUM(), [normal(17.5,2.5) for x in range(100)])
    print(f"{n.mu:g} {n.sd:g}")

  def data(_):
    d=DATA().csv(the.train)
    random.shuffle(d.rows)
    d.sort()
    for cols in [d.cols.x,d.cols.y]:
      print("")
      for col in cols: print(col)
    print(show(some([d.loglike(row,1000,2) for row in d.rows],5)))
    for j,row in enumerate(d.rows):
      if j % 30 == 0 : print(row)

  def hnr(_):
    d = DATA().csv(the.train)
    random.shuffle(d.rows)
    hitnrun(d)

random.seed(the.seed)
[getattr(eg, s[1:], eg.noop)(i+1) for i,s in enumerate(sys.argv)]
