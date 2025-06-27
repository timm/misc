"""
abc.py: tiny acive learning,  multi objective.
(c) 2025 Tim Menzies, <timm@ieee.org>. MIT license

Options, with (defaults):
 -f file   : data name (../../moot/optimize/misc/auto93.csv)
 -s seed   : set random number rseed (123456781)
 -F Few    : a few rows to explore (512)
 -p p      : distance calcs: set Minkowski coefficient (2)

Bayes:
 -k k      : bayes hack for rare classes (1)
 -m m      : bayes hack for rare attributes (2)

Active learning:
 -a acq    : xploit or xplore or adapt (xploit)
 -A Assume : on init, how many initial guesses? (4)
 -B Build  : when growing theory, how many labels? (20)
 -C Check  : when testing, how many checks? (5) 
 -g guess  : ratio for sampling(0.5) """

import math, random, sys, re

def atom(s):
  for fn in [int, float]:
    try: return fn(s)
    except: pass
  s = s.strip()
  tmp = s.lower()
  return True if tmp=="True" else (False if tmp=="False" else s)

def csv(file):
  with open(file) as f:
    for s in f:
      if s.strip(): yield [atom(x) for x in s.strip().split(",")]

class o:
  __init__ = lambda i, **d: i.__dict__.update(d)
  __repr__ = lambda i     : f"{i.__class__.__name__}{vars(i)}"

the = o(**{k:atom(v) for k,v in re.findall(
                       r"\w+\s+(\w+)[^\(]*\(\s*([^)]+)\)",__doc__)})

#--------------------------------------------------------------------
class Sym(o):
  def __init__(i, at=0, txt=""): i.at,i.txt,i.n,i.has=at,txt,0,{}

  def bin(i,x): return x
  def add(i,x,inc=1,_=False): 
    if x!="?": 
      i.n+=inc; i.has[x]=i.has.get(x,0)+inc

  def pdf(i,s, prior=0):
    return (i.has.get(s,0) + the.m*prior) / (i.n + the.m + 1/Num.big)

#--------------------------------------------------------------------
class Num(o):
  big = 1e32
  def __init__(i, at=0, txt=""):
    i.at,i.txt,i.n = at,txt,0
    i.mu, i,m2,i.sd = 0,0,0
    i.lo,i.hi = i.big, -i.big
    i.heaven = 0 if txt.endswith("-") else 1

  def norm(i,x): return (x-i.lo)/(i.hi-i.lo+1/Num.big)

  def add(i,x, inc=1,_=False):
    if x!="?": 
      i.n+=inc; i.lo=min(i.lo,x); i.hi=max(i.hi,x)
      if inc < 0 and i.n < 2:
        i.sd = i.m2 = i.mu = i.n = 0
      else:
       d     = x - i.mu
       i.mu += inc * (x / i.n)
       i.m2 += inc * (d * (x - i.mu))
       i.sd  = 0 if i.n <= 2 else (max(0,i.m2)/(i.n-1))**.5

  def pdf(i,v,_):
    sd  = i.sd or 1 / Num.big
    var = 2 * sd * sd
    z   = (v - i.mu) ** 2 / var
    return min(1, max(0, math.exp(-z) / (2 * math.pi * var) ** 0.5))

#--------------------------------------------------------------------
class Cols(o):
  def __init__(i,names):
    i.names,i.all,i.x,i.y = names,[],[],[]
    i.klass = None
    for n,t in enumerate(names):
      c = Num(n,t) if t[0].isupper() else Sym(n,t)
      i.all.append(c)
      if t[-1] != "X":
        (i.y if t[-1] in "!+-" else i.x).append(c)
        if t[-1] == "!": i.klass = c

#--------------------------------------------------------------------
class Data(o):
  def __init__(i, src=[]): 
    i.rows,i.cols = [],None
    [i.add(x) for x in src]

  def sub(i,t,zap=False): return i.add(t,-1,zap)

  def add(i,t, inc=1, zap=False):
    if not i.cols: i.cols = Cols(t)
    else:
      if inc > 0 : i.rows.append(t)
      elif zap   : i.rows.remove(t) # slow for large lists
      for col in i.cols.all: col.add(t[col.at], inc)
    return t

  def clone(i,rows=[]): return Data([i.cols.names]+rows)

  def like(i, t, nall=100, nh=2):
    prior = (i.n + the.k) / (nall + the.k*nh)
    tmp = [c.pdf(v,prior) for c in i.cols.x if (v:=t[c.at]) != "?"]
    return sum(math.log(n) for n in tmp + [prior] if n>0)

  def ydist(i,r):
    d = sum((c.norm(r[c.at])-c.heaven)**2 for c in i.cols.y)
    return (d/len(i.cols.y))**0.5

  def ydists(i,t=None):
    return  sorted(t or i.rows, key=lambda r: i.ydist(r)) # best at 0

#--------------------------------------------------------------------
def acquire(yes, no, t, nall=100, nh=2):
  b = math.exp(yes.like(t, nall, nh))
  r = math.exp(no.like(t, nall, nh))
  p = nall / the.build
  q = dict(xploit=0, xplor=1).get(the.Acq, 1 - p)
  return (b + r*q) / abs(b*q - r + 1 / Num.big)

# best, rest = split(initial)
# while not enough points:
#   guess scores for top candidates in todo
#   pick the best guess (hi)
#   add hi to best
#   remeasure distances
#   if best is too big:
#     move worst from best to rest
def acquires(data, rows):
  random.shuffle(rows)
  nall     = the.Assume
  cut      = round(nall**the.Guess) 
  todo     = rows[nall:]
  done     = data.clone(data.ydists(rows[:nall]))
  best     = data.clone(done.rows[:cut])
  rest     = data.clone(done.rows[cut:])
  _guess   = lambda t: -acquire(best,rest, t, nall, 2) # smaller is better
  _guesses = lambda t: sorted(t, key=_guess) # best at 0 
  while len(todo) > 2 and nall < the.Build:
    nall  += 1
    hi,*lo = _guesses(todo[:the.Few*2]) # best at start
    todo   = lo[:the.Few] + todo[the.Few*2:] + lo[the.Few:]
    done.add( best.add(hi))
    best.rows = done.ydists(best.rows)
    while len(best.rows) >= cut:
      rest.add( best.sub( best.rows.pop(-1)))
  return o(best = best.rows[0], 
           labelled=done.rows, 
           test = data.ydists(_guesses(todo)[:the.check])[0])

#--------------------------------------------------------------------
def eg__acquires():
  print("== testing acquires")
  names = ["x1", "x2", "y!"]
  d = Data(); d.add(names)
  for _ in range(30):
    row = [random.uniform(0,1),
           random.uniform(0,1),
           "a" if random.random() < 0.5 else "b"]
    d.add(row)
  s = Syms(d)
  best, rest = s.acquires(d.rows)
  print(f"#best={best.nall}  #rest={rest.nall}")
  print("best.y dist:", sum(d.ydist(r) for r in d.rows[:best.nall]) / best.nall)
  print("rest.y dist:", sum(d.ydist(r) for r in d.rows[-rest.nall:]) / rest.nall)

#--------------------------------------------------------------------
def cli(arg,d):
  for k in d:
    if len(arg)> 1 and arg[1] == k[0]: d[k]=atom(arg)

if __name__ == "__main__":
  for arg in sys.argv:
    cli(arg, the.__dict__)
    if (fn := globals().get("eg__"+arg)):
      random.seed(the.seed)
      fn()
