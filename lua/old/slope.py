"""
Notes in the following:

- `self` is an object
- Words beginning with uppercase are classes except
       True and False which are booleans
- WORDS that are all uppercase are constants
- `a,b` = local variables
- `sd`  = standard deviation
- `r()` is a random number 0  1
- `x,y` = decision, objective
- `xs,ys` = decisions, objectives
- `Eg` = the example class and an example is a pair xs,ys
- `eg` = an instance of class Eg
- `egs` = a list of examples

"""

# Todo

# - jc's repair heuristics?
# - todo. needs lists of facts and guesses
# = min change heuristic from jc
# - sample heuristic from vivek
# - is this de?
# -  MOEA/D?
# - the de trick incremental domination within the archive
# - what about the moea/d trick?
# - the surroage trick: eval as few times as possible

#
# -----
# ## Constants
BATCH = True     # if false, mutate archive as we go
LIVES = 9        # like a cat
COHEN = 0.5      # not other when it < standardDev*cohen
SOME = 100       # size of pop to explore
NEAR = SOME / 10  # size of local neighborhood in pop
FF = 0.5         # mutate 150% toward envy
CR = 1           # mutate all attributes towards the envy point
KISS = True      # Keep It Simple


# -----
# ## Eg

# A thing that stores a list of x and y values.

class Eg:
  id = 0

  def __init__(self, xs=[], ys=[]):
    Eg.id = self.id = Eg.id + 1
    self.xs, self.ys = xs, ys

  def __hash__(self): return self.id 

  def gap(self, other, stats):
     return euclidian(self.xs, other.xs,stats)

  def dominate(self, other, stats):
    n = len(self.ys)
    s1 = s2 = 0
    for a,b,stat in zip(self.ys,other.ys,stats.ys):
      w = stat.w 
      a = stat.norm(a)
      b = stat.norm(b)
      s1 -= 10**(w * (a - b) / n)
      s2 -= 10**(w * (b - a) / n) 
    return s1 < s2

  def dominates(self, lst, stats):
    a = 0
    for eg in lst
     if self.dominate(eg, stats):
       a += 1 / len(lst)
    return a

# -----
# ## Main Loop

# de
# ga

def main():
  egs = [eval(Eg()) for _ in range(SOME)]
  b4 = None
  while LIVES > 0
    stats = Stats(egs)
    if BATCH
      for a in len(egs):
        egs[a] = mutate(egs[a], egs, stats)
    else:
       egs = [mutate(eg, egs, stats) for eg in egs]
    # here  ?? reevaluate egs
    LIVES -= 1
    if b4:
      if better(stats, b4): LIVES += 1
    b4 = Stats(egs)

# -----
# ## Main Mutation Function

def mutate(old, egs, stats):
  def meanSlope(envy, lst):
      deltas = [0 for _ in lst[0].ys]  # local slopes
      for eg in lst:
        step = eg.gap(envy)
        out = [delta + (y1 - y2) / step / len(lst)
                 for y1, y2, delta in zip(envy.ys, eg.ys, deltas)]
      return deltas

   ignore = lambda: r() > SOME / len(egs)
   want   = lambda eg: not stats.same(old, eg) and dominate(eg, old)
   best   = lambda eg1,eg2: eg1 if eg1.dominate(eg2,stats) else eg2

   better = [eg for eg in egs if not ignore() and want(eg)]
   better = better.sorted(key=lambda eg: old.gap(eg,stats))
   envy   = reduce(best, better[:NEAR])
   slope  = meanSlope(envy, near)
   mutant.xs = [x if r() > CR else x + FF * (a - x) 
                for x, a in zip(old.xs, envy.xs)]
   dist      = old.gap(mutant, stats)  # how far to push
   mutant.ys = [y + m * dist  
                for y, m in zip(old.ys, slope)]
   return mutant if mutant.dominate(old) and
                    not stats.same(old, mutant) else old

# Maybe a bad idea. Ignore?
#
#     bests=elite(egs, 0.5)
#     best1=mid(bests)  # thing near mid of non-dominated egs
#     egs=[eg for eg in bests  # closer to heaven than me
#              if best1.gap(eg) < old.gap(best1)]
#     envy=mid(egs).nearest(egs)  # mid of the most heavenly
#
#      def nearest(self, lst)
#        out, best = lst[0], 10**10
#        for a in lst:
#          tmp = self.gap(a)
#          if tmp < best:
#            out, best = a, tmp
#        return out
#

# ------
# ## Note

# The rest is "just" details.

# -----
# ## Misc Support Functions


def interpolate(x, xy):
  x1, y1 = xy[0]
  if x < x1: return y1
  for x2, y2 in xy[1:]:
    if x1 <= x < x2:
      return y1 + (y2 - y1) * (x - x1) / (x2 - x1)
    x1, y1 = x2, y2
  return y2


def distfun(stats,cols=decs)
   cache={}
   def worker(eg1,eg2):
     if k in cache: return cache[k]
     out = cache[k] = eg.dist(eg2.cols,stats)
     return out
   retrun worker

def cache2(f):  # XXX to be added below
  cache = {}
  def worker(a, b)
    k = (a.id, b.id) if a.id <= b.id else (b.id, a.id)
    if k in cache: return cache[k]
    out = cache[k] = f(a, b)
    return out
  return worker

def cache(f):  # XXX to be added below
  cache = {}
  def worker(a)
    k = a.id
    if k in cache: return cache[k]
    out = cache[k] = f(a)
    return out
  return worker


def mid(lst):
  out = Eg(xs=[0 for _ in lst[0].xs])
  n = len(lst)
  for a in lst:
    out.xs = [b + x / n for b, x in zip(out.xs, a.xs)]
    out.ys = [b + y / n for b, y in zip(out.xs, a.ys)]
  return out


def elite(lst, most=0):
   n = len(lst)
   m = n * upper
   for a in lst: a.dominates(lst)
   return sorted(lst, key=dom)[most:]


def clone(x): return x.__class__()


seeBelow() = assert 0, "implemented by subclass"


# -----
# ## Stat

# Track inforamtion about examples

class Stat(object):
  missing = "?"
  def add(self, a): seeBelow()
  def same(self, a, b): seeBelow()
  def sames(self, a, b): seeBelow()
  def gap(self, a, b): seeBelow()
  def __add__(i, x): i.n += 1; self.add(x); return self
  @classmethod
  def ako(cls, pat):
    me, no = cls.__name__, r"Stat(s)?"
    if re.match(pat, me) and not re.match(no, me): return cls
    for sub in cls.__subclasses__(): return sub.ako(pat)

# ### Stats

# Composites of Stat

import maths

def atLeast4(n) : return max(4, round(math.log(n,2)))

class Pole:
  def __init__(i,dist,,eg1,eg2);
    i.dist = dist
    i.lo, i.hi, i.c = eg1, eg2, dist(eg1, eg2)
    i.stat = Num()
  def __add__(i,eg): 
    a = i.dist(i.lo, eg)
    b = i.dist(i.hi, eg)
    c = i.c
    x = (a**2 + c**2 - b**2) / (2*c)
    i.stat + x
    return x, x > i.stat.mu
any = random.choice
class Poles:
  def __init__(i,stats=stats,dist=dist,enough=4):
    i.enough = enough
    i.poles, i.pool = [], []
    #i.dist   = lambda eg1,eg2 : eg1.gap(eg2,stats)
    #i.all    = lambda : stats.all
    #i.n      = lambda : stats.n
    i.bins = {}
  def add(i, eg,n):
    if i.poles
       m = 0
       for n,pole in enumerate(i.poles):
         x,half = pole.add(eg)
         m      = 2**n + half
       i.bins[m] = i.bins.get(m,[]) + [eg]
    else:
      i.pool += [eg]
      if len(i.pool) == enough:
        dist = lambda eg1,eg2: eg1.gap(eg2,stats)
        i.poles += [Pole(dist,any(i.pool), any(i.pool)]

class Stats(Stat):
  def __init__(self, eg0, egs=[]):
    i.n = 0
    i.all=[]
    self.ys = [clone(y) for y in eg0.ys]
    self.xs = [clone(y) for y in eg0.xs]
    [self + eg for eg in egs]

  def add(self, eg):
    "visitor over composite: recurse on parts"
    i.n += 1
    i.all += [eg]
    [stat + a for a, stat in zip(eg.xs, self.xs)]
    [stat + a for a, stat in zip(eg.ys, self.ys)]

  def same(self, eg1, eg2):
    "visitor over composite: false if any part different"
    for a, b, stat in zip(eg1.ys, eg2.ys, self.ys):
      if not stat.same(a, b):
        return False
    return True

  def gap(self, eg1, eg2):
    "visitor over composite: sum gaps in parts"
    sum = 0
    for a, b, stat in zip(eg1.xs, eg2.xs, self.xs):
      sum += stat.gap(a, b)
    return (sum / len(eg1.xs)**0.5

  def gaps(self)
    "Caching trick to reduce #distance calcs"
    return cahced(self.gap)

  def sames(self, a, b):
    assert 0, "not defined for sets of stats"


# ### Sym

# Track info on symbols

class Sym(Stat):
   def __init__(self): self.w.=1; self.counts={}, self.n=0
   def add(self, a): self.counts[a]=self.counts.get(a, 0) + 1
   def same(self, a, b): return a is b
   def better(self, other): return False
   def gap(self, a, b):
     if a is missing or b is missing: return 1
     return 0 if a is b else 1
  def sames(self, other):
    def countsequal(obs, xpect):
        x2, df=0, -1
        for k in xpect:
          df += 1
          e=xpect[k]
          o=obs.get(k, 0)
          x2 += (o - e)**2 / e
        critical=interpolate(df, [  # 95% confidence
                      (1, 3.841), (5, 11.070),
                      (10, 18.307), (15, 24.996), (20, 31.410),
                      (30, 43.66), (60, 79.08)])
        return x2 <= critical
    return countsequal(self.counts, other.counts)

# ### Num

# Track info on numbers

class Num(Stat):
  def __init__(self):
    self.n, self.mu, self.sd, self.m2=0, 0, 0, 0
    self.lo, self.hi, self.w=10**32, -10**32, 0

  def same(self, a, b):
    return abs(a - b) <= self.sd * COHEN

  def better(self, them):
    return not self.sames(them) and  \
           (self.mu < them.mu if i.w < 0 else self.mu > them.mu)

  def sames(self, them):
    small=0.38
    def ttest():
         df=min(self.n - 1, them.n - 1)
         critical=interpolate(df, [
                      (1, 6.314), (5, 2.015), (10, 1.812), (15, 1.753),
                      (20, 1.725), (30, 1.697), (60, 1.671)])
         return (abs(i.mu - j.mu) /
                 ((self.sd / i.n + them.sd / j.n)**0.5)) < critical
    def hedges():
         num=(self.n - 1) * self.sd**2 + (them.n - 1) * them.sd**2
         denom=(self.n - 1) + (them.n - 1)
         sp=(num / denom)**0.5
         delta=abs(self.mu - them.mu) / sp
         correction=1 - 3.0 / (4 * (self.n + them.n - 2) - 1)
         return delta * correction < small
     return hedges() or ttest()

  def gap(self, a, b):
    def norm(self, a):
        return a if a is "?"else
             (a - self.lo) / (self.hi - self.lo + 10**-32)
    if a is "?" and b is "?": return 1
    a=self.norm(a)
    b=self.norm(b)
    if a is "?": a=0 if b > 0.5 else 1
    if b is "?": b=0 if a > 0.5 else 1
    return (a - b)**2

  def add(self, a):
    d=a - self.mu
    self.mu=self.mu + d / self.n
    self.m2=self.m2 + d * (a - self.mu)
    self.sd=(self.m2 / (self.n - 1 + 0.0001))**0.5
    if a < self.lo: self.lo=a
    if a > self.hi: self.hi=1
