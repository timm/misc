"""
abc.py: tiny acive learning, multi objective.
(c) 2025 Tim Menzies, <timm@ieee.org>. MIT license

 -h              show help
 -A Assume=4     on init, how many initial guesses?
 -B Build=24     when growing theory, how many labels?
 -C Check=5      when testing, how many checks? 
 -F Few=512      just explore a Few rows
 -a acq=xploit   xploit or xplor or adapt
 -g guess=0.5    |hot| is |lit|**guess
 -k k=1          bayes hack for rare classes 
 -m m=2          bayes hack for rare attributes
 -p p=2          distance calcs coeffecient
 -s seed=1234567891 
 -f file=../../../moot/optimize/misc/auto93.csv
"""
import math, random, sys, re
from types import SimpleNamespace as o

def atom(x):
  "coerce str -> atom"
  try: return int(x)
  except: 
    try: return float(x)
    except: return x

the  = o(**{k:atom(v) for k,v in re.findall(r"(\w+)=(\S+)",__doc__)})
#---------------------------------------------------------------------
class T:
  "Root class with generic print"
  __repr__ = lambda i : f"{i.__class__.__name__}{vars(i)}"
  sub      = lambda i,t,zap=False: i.add(t,-1,zap)
#--------------------------------------------------------------------
class Sym(T):
  "Summarize symbolic columns."
  def __init__(i, inits=[], at=0, txt=""): 
    i.at,i.txt,i.n,i.has=at,txt,0,{}
    [i.add(x) for x in inits]

  def add(i, x,inc=1,**_):
    "Update"
    if x!="?": i.n+=inc; i.has[x]=i.has.get(x,0)+inc

  def like(i,s, prior=0):
    "Probalistic membership"
    print(i.n)
    return (i.has.get(s,0) + the.m*prior) / (i.n + the.m + 1/Num.big)

#--------------------------------------------------------------------
class Num(T):
  "Summarize numeric columns."
  big = 1e32
  def __init__(i,inits=[], at=0, txt=""):
    i.at,i.txt,i.n   = at,txt,0
    i.mu, i.m2, i.sd = 0,0,0
    i.lo,i.hi        = i.big, -i.big
    i.heaven         = 0 if txt.endswith("-") else 1
    [i.add(x) for x in inits]

  def norm(i,x): 
    "num -> 0..1"
    return (x-i.lo)/(i.hi-i.lo+1/Num.big)

  def win(i,x): 
    "Normalized distance mu to lo."
    return (1- (x-i.lo)/(i.mu-i.lo))

  def add(i, x, inc=1,**_):
    "Update"
    if x!="?": 
      i.n += inc; i.lo=min(i.lo,x); i.hi=max(i.hi,x)
      if inc < 0 and i.n < 2: i.sd = i.m2 = i.mu = i.n = 0
      else:
        d     = x - i.mu
        i.mu += inc * (d / i.n)
        i.m2 += inc * (d * (x - i.mu))
        i.sd  = 0 if i.n <= 2 else (max(0,i.m2)/(i.n-1))**.5

  def like(i,v,_):
    "Probalistic membership"
    sd  = i.sd or 1 / Num.big
    var = 2 * sd * sd
    z   = (v - i.mu) ** 2 / var
    return min(1, max(0, math.exp(-z) / (2 * math.pi * var) ** 0.5))
#--------------------------------------------------------------------
class Data(T):
  "Summarize rows into their various columns."
  def __init__(i, src=[]): 
    i.n,i.rows,i.cols = 0,[],None
    [i.add(x) for x in src]

  def add(i, t, inc=1, zap=False):
    "Update"
    if not i.cols: i.cols = _cols(t)
    else:
      i.n += inc
      if inc > 0 : i.rows.append(t)
      elif zap   : i.rows.remove(t) # slow for large lists
      for col in i.cols.all: col.add(t[col.at], inc)
    return t

  def clone(i,rows=[]): 
    "Make a new data using old structure."
    return Data([i.cols.names]+rows)

  def like(i, t, nall=100, nh=2):
    "how much does self like `t`"
    prior = (i.n + the.k) / (nall + the.k*nh)
    tmp = [_xx(c.like(v,prior)) for c in i.cols.x if (v:=t[c.at]) != "?"]
    return sum(math.log(n) for n in tmp + [prior] if n>0)

  def ydist(i,r):
    "Distance to heaven"
    d = sum((c.norm(r[c.at])-c.heaven)**(the.p) for c in i.cols.y)
    return (d/len(i.cols.y))**(1/the.p)

  def ydists(i,t=None):
    "Sort by distance to heaven."
    return  sorted(t or i.rows, key=lambda r: i.ydist(r)) 

def _xx(x): print(round(x,2)); return x
def _cols(names):
  "Factory for making Nums and Syms."
  x, y, all, klass = [], [], [], None
  for n,t in enumerate(names):
    col = (Num if t[0].isupper() else Sym)(at=n,txt=t)
    all.append(col)
    if t[-1] != "X":
      if t[-1] == "!": klass = col
      (y if t[-1] in "!+-" else x).append(col)
  return o(names=names, all=all, x=x, y=y, klass=klass)
#--------------------------------------------------------------------
def acquire(yes, no, t, nall=100, nh=2):
  b = math.exp(yes.like(t, nall, nh))
  r = math.exp(no.like(t, nall, nh))
  return b > r
  #p = nall / the.Build
  #q = dict(xploit=0, xplor=1).get(the.acq, 1 - p)
  #return (b + r*q) / abs(b*q - r + 1 / Num.big)

def acquires(data, rows):
  """dim  = unknown (unlabeled) pool
     lit  = known (labeled) data. Lit divides into hot and dull.
     hot  = lit's top-ranked known items 
     dull = lit's remaining known items (less informative)"""
  random.shuffle(rows)
  # Split data: known (lit) and unknown (dim)
  dim  = rows[the.Assume:]                  # unknown, lacking labels
  lit  = data.clone(rows[:the.Assume])      # known, already labeled
  # Sort known items by outcome-based distance
  lits = lit.ydists()
  # Determine how many should be in the "hot" set
  _nhot = lambda: int(len(lit.rows)**the.guess)
  # Partition lit into hot (best) and dull (rest)
  hot  = data.clone(lits[:_nhot()])         # best known
  dull = data.clone(lits[_nhot():])         # rest of the known

  # Scoring function: prioritize items that could reshape the boundary
  _score  = lambda t: -acquire(hot, dull, t, len(lit.rows), 2)
  # Rank unknown items by score (best first)
  _ranked = lambda t: sorted(t, key=_score)

  # Active learning loop: label most promising unknowns
  while len(dim) > 2 and len(lit.rows) < the.Build:
    hi, *lo = _ranked(dim[:the.Few * 2])       # top guess and backups
    # Keep a few top ranked items, cycle the rest to the end
    dim = lo[:the.Few] + dim[the.Few * 2:] + lo[the.Few:]
    # Label high-potential item
    lit.add(hot.add(hi))
    # Re-sort hot based on updated knowledge
    hot.rows = lit.ydists(hot.rows)
    # Keep hot size within bounds; move any overflow to dull
    while len(hot.rows) > _nhot():
      dull.add( hot.sub( hot.rows.pop(-1)))

  return o(hot=hot.rows, 
           dull=dull.rows, 
           test=data.ydists(_ranked(dim)[:the.Check])) 
#--------------------------------------------------------------------
def extend(kl,s):
  return lambda f: (setattr(kl,f.__name__,f), setattr(f,"__doc__",s))

def cli(d, args):
  for n,arg in enumerate(args):
    if len(arg) > 1:
      for k in d:
        if arg[1] == k[0]: 
          d[k] = atom(args[n+1])
      if (fn := globals().get(f"eg{arg.replace('-', '_')}")):
        random.seed(the.seed)
        fn()

def csv(file):
  with open(file) as f:
    for s in f:
      if s.strip(): yield [atom(x) for x in s.strip().split(",")]

def eg_h()    : print(__doc__)
def eg__the() : print(the)
def eg__csv() : [print(t) for t in csv(the.file)]
def eg__sym() : print(Sym("aaaabbc").has)
def eg__num() : print(Num(random.gauss(10,2) for _ in range(1000)).sd)
def eg__data(): [print(col) for col in Data(csv(the.file)).cols.all]

def eg__addsSubs():
  d1 = Data(csv(the.file))
  d2 = d1.clone()
  x  = d2.cols.x[1]
  for row in d1.rows:
    d2.add(row)
    if len(d2.rows)==100: mu1,sd1 = x.mu,x.sd
  for row in d1.rows[::-1]:
    if len(d2.rows)==100: mu2,sd2 = x.mu,x.sd
    d2.sub(row,True)
  assert abs(mu2/mu1) < 1.01 and abs(sd2/sd1) < 1.01

def eg__bayes():
  data = Data(csv(the.file))
  assert all(-20 <= data.like(t) <= 0 for t in data.rows)
  print(sorted([round(data.like(t),2) for t in data.rows])[::20])

def eg__ydist():
  data = Data(csv(the.file))
  assert all(0 <= data.ydist(t) <= 1 for t in data.rows)
  print(sorted([round(data.ydist(t),2) for t in data.rows])[::20])

def eg__acquires():
  data = Data(csv(the.file))
  print("!!!!!!!",the.file)
  R = lambda z: f" {z:.2f}".lstrip("0")
  Y = lambda t: data.ydist(t)
  hot,test,b4 = Num(),  Num(), Num(Y(t) for t in data.rows)
  for _ in range(20):
     x = acquires(data,data.rows)
     hot.add( Y(x.hot[0]))
     test.add( Y(x.test[0]))
  print(R(b4.win(hot.mu)), 
        *[f"{len(z):>5}" for z in [data.rows, data.cols.x, data.cols.y]],
        *[R(z) for z in [b4.mu, b4.lo,hot.mu, test.mu]], 
        *[R(z) for z in [b4.win(hot.mu), b4.win(test.mu)]],
        re.sub(r"^.*/"," ",the.file), sep=",")
#--------------------------------------------------------------------
if __name__ == "__main__": cli(the.__dict__,sys.argv)
