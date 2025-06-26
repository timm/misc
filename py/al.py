import math, random, sys

class o:
  __init__ = lambda i, **d: i.__dict__.update(d)
  __repr__ = lambda i     : f"{i.__class__.__name__}{vars(i)}"

the = o(Acq="xploit", assume=4, Bins=4, build=20, check=5,
        file="", guess=0.5, k=2, m=1, p=2, seed=1234567891)

#--------------------------------------------------------------------
class Sym(o):
  def __init__(i, at=0, txt=""): i.at,i.txt,i.n,i.has=at,txt,0,{}

  def bin(i,x): return x
  def add(i,x): 
    if x!="?": i.n+=1; i.has[x]=i.has.get(x,0)+1

#--------------------------------------------------------------------
class Num(o):
  big = 1e32
  def __init__(i, at=0, txt=""):
    i.at,i.txt,i.n = at,txt,0
    i.lo,i.hi = i.big, -i.big
    i.heaven = 0 if txt.endswith("-") else 1

  def norm(i,x): return (x-i.lo)/(i.hi-i.lo+1/i.big)
  def bin(i,x): return int(min(the.Bins-1,i.norm(x)*the.Bins))
  def add(i,x):
    if x!="?": i.n+=1; i.lo=min(i.lo,x); i.hi=max(i.hi,x)

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

  def add(i,t):
    for c in i.all: c.add(t[c.at])
    return t

#--------------------------------------------------------------------
class Data(o):
  def __init__(i, src=[]): 
    i.rows,i.cols = [],None
    [i.add(x) for x in src]

  def add(i,t):
    if i.cols: i.rows += [i.cols.add(t)]
    else: i.cols = Cols(t)
    return i

  def clone(i,rows=[]): return Data([i.cols.names]+rows)

  def ydist(i,r):
    d = sum((c.norm(r[c.at])-c.heaven)**2 for c in i.cols.y)
    return (d/len(i.cols.y))**0.5

#--------------------------------------------------------------------
class Syms(o):
  def __init__(i, d):
    i.data, i.nall, i.nh, i.nk = d, 0, 0, {}
    i.fs = {}  # flat dict: (k, at, bin) → count

  def adds(i, rows, k, inc=1): 
    [self.add(r,k,inc) for r in rows]; return rows

  def add(i, r, k, inc=1):
    i.nall += inc
    i.nh += k not in i.nk
    i.nk[k] = i.nk.get(k, 0) + inc
    for c in i.data.cols.x:
      if (v := r[c.at]) != "?":
        key = (k, c.at, c.bin(v))
        i.fs[key] = i.fs.get(key, 0) + inc
    return r

  def sub(i, r, k): return i.add(r, k, inc=-1)

  def like(i, r, k):
    prior = (i.nk[k] + the.k) / (i.nall + the.k * i.nh)
    l = math.log(prior)
    for c in i.data.cols.x:
      if (v := r[c.at]) != "?":
        key = (k, c.at, c.bin(v))
        f = i.fs.get(key, 0)
        l += math.log((f + the.m * prior) /
                      (i.nk[k] + the.m + 1 / Num.big))
    return l

  def score(i, r):
    b = math.exp(i.like(r, True))
    u = math.exp(i.like(r, False))
    p = i.nall / the.build
    q = dict(xploit=0, xplor=1).get(the.Acq, 1 - p)
    return (b + u*q) / abs(b*q - u + 1 / Num.big)

  def acquires(i, rows):
    rows = rows[:]
    random.shuffle(rows)
    seen = Syms(i.data) # use all x variables for discretimzation/normalizatipn
    todo = rows[the.assume:]
    done = i.clone(rows[:the.assume])
    Y    = lambda r: done.ydist(r) # use done Y variables for ydist
    done.rows = sorted(done.rows, key=Y, reversed=True)
    cut  = int(the.assume ** the.guess)
    best = seen.adds(done.rows[cut:],True)
    rest = seen.adds(done,rows[:cut],False)
    while len(todo)>2 and (len(best) + len(rest)) < the.Build:
      hi,tmp = extractMost(todo[:the.Few], seen.score)
      todo   = todo[the.Few:] + tmp
      insertDescending(best, hi, Y)
      if len(best) > (len(best) + len(rest)) > XXX:
        demoted = best.pop()
        rest += [demoted]
        seen.sub(demoted,True)
        seen.add(demoted,False)
    return best, rest

#--------------------------------------------------------------------
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

def extractMost(lst, key):
  most = key(lst[-1])
  for i in range(len(lst) - 1):
    if (s := key(lst[i])) > most:
      lst[i], lst[-1] = lst[-1], lst[i]
      most = s
  return lst.pop(), lst

def insertDescending(lst, new, key):
  lst.append(lst[-1])  # open a slot at the end
  i = len(lst) - 2
  while i >= 0 and key(new) > key(lst[i]):
    lst[i + 1] = lst[i]
    i -= 1
  lst[i + 1] = new
  return new
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
if __name__ == "__main__":
  for arg in sys.argv:
    if (fn := globals().get("eg__"+arg)):
      random.seed(the.seed)
      fn()
