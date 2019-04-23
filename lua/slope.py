"""
Notes in the following:

- `self` is an object
- Words beginning with uppercase are classes except
       True and False which are booleans
- WORDS that are all uppercase are constants
- `a,b` = local variables
- `sd`  = standard deviation
- `r()` is a random number 0..1
- `x,y` = decision, objective
- `xs,ys` = decisions, objectives
- `Eg` = the example class and an example is a pair xs,ys
- `eg` = an instance of class Eg
- `egs` = a list of examples

"""

# jc's repair heuristics?
# -----
BATCH = True     # if false, mutate archive as we go
LIVES = 9        # like a cat
COHEN = 0.5      # not other when it < standardDev*cohen
SOME = 100       # size of pop to explore
NEAR = SOME / 10  # size of local neighborhood in pop
FF = 0.5         # mutate 150% toward envy
CR = 1           # mutate all attributes towards the envy point
KISS = True      # Keep It Simple

# # Text
#
# asdasd asdas das as asddasasd
# asdasd asdas das as asddasasd
# asdasd asdas das as asddasasd
# asdasd asdas das as asddasasd


# -----
def clone(x): return x.__class__()

seeSubclass= assert 0,"implemented by subclass"

class Col(object):
  def __add__(i,x): i.n += 1;  self.add(x); return self
  def add(self, a)     : seeSubclass()
  def same(self, a, b): seeSubclass()
  def norm(self, a):   : seeSubclass()
  def gap(self, a, b)  : seeSubclass()

class Num(Col):
  def __init__(self):
    self.n, self.mu, self.sd, self.m2 = 0, 0, 0, 0
    self.lo, self.hi = 10**32, -10**32

  def same(self, a, b):
    return abs(a - b) <= self.sd * COHEN

  def norm(self, a):
    return a if a is "?"else
           (a - self.lo) / (self.hi - self.lo + 10**-32)

  def gap(self, a, b):
    if a is "?" and b is "?": return 1
    a = self.norm(a)
    b = self.norm(b)
    if a is "?": a = 0 if b > 0.5 else 1
    if b is "?": b = 0 if a > 0.5 else 1
    return (a - b)**2

  def add(self, a):
    d = a - self.mu
    self.mu = self.mu + d / self.n
    self.m2 = self.m2 + d * (a - self.mu)
    self.sd = (self.m2 / (self.n - 1 + 0.0001))**0.5
    if a < self.lo: self.lo = a
    if a > self.hi: self.hi = 1


class Sym(Col):
   def __init__(self): i.n = 0
   def add(self, a): pass
   def same(self, a, b): return a is b
   def norm(self, a): return a
   def gap(self, a, b):
     if a is "?" or b is "?": return 1
     return 0 if a is b else 1

# -----


class Stats:
  def __init__(self, eg0, egs):
    self.ys = [clone(y) for y in eg0.ys]
    self.xs = [clone(y) for y in eg0.xs]
    for eg in egs:
      [stat + a for a, stat in zip(eg.xs, self.xs)]
      [stat + a for a, stat in zip(eg.ys, self.ys)]

  def same(self, eg1, eg2):
    for a, b, stat in zip(eg1.ys, eg2.ys, self.ys):
      if not stat.same(a, b):
        return False
    return True

  def better(self, other):
    "stats ehre"

  def gap(self, eg1, eg2):
    sum = 0
    for a, b, stat in zip(eg1.xs, eg2.xs, self.xs):
      sum += stat.gap(a, b)
    return (sum / len(eg1.xs)**0.5

  def gapfun(self):  # XXX to be added below
    cache={}
    def worker(eg1, eg2)
      k1, k2=eg1.id, eg2.id
      k=(k1, k2) if k1 < k2 else (k2, k1)
      if k in cache: return cache[k]
      out=cache[k]=self.gap(eg1, eg2)
      return out
    return worker


# min change heuristic from jc
# sample heuristic from vivek

# is this de?


# -----
class Eg:
  id=0
  def __init__(self, xs=[], ys=[]):
    Item.id=self.id=Item.id + 1
    self.xs, self.ys=xs, ys
  def gap(self, other):
     return euclidian(self.xs, other.xs)
  def dominate(self, a, stats):
    return t, f
  def dominates(self, lst, stats):
    a= 0
    for eg in lst
     if self.dominate(eg, stats):
       a += 1 / len(lst)
    return a
  def dom(self):
     return Eg.items.get(self.id, 0)
  def nearest(self, lst)
    out, best=lst[0], 10**10
    for a in lst:
      tmp=self.gap(a)
      if tmp < best:
        out, best=a, tmp
    return out

# -----
# -----
def mid(lst):
  out=Eg(xs=[0 for _ in lst[0].xs])
  n=len(lst)
  for a in lst:
    out.xs=[b + x / n for b, x in zip(out.xs, a.xs)]
    out.ys=[b + y / n for b, y in zip(out.xs, a.ys)]
  return out

# -----
def elite(lst, most=0):
   n=len(lst)
   m=n * upper
   for a in lst: a.dominates(lst)
   return sorted(lst, key=dom)[most:]

# repair constraint violations
# cost to eval
# cost to check constraint violations
# what is the essence of flash
# what is the essence of dodge

# -----
def mutate(old, egs, stats):
   want = lambda eg: not stats.same(old, eg) and dominate(eg, old)
   ignore = lambda: r() > SOME / len(egs)
   egs    = [eg for eg in egs if not ignore() and want(eg)]
   egs    = egs.sorted(key=lambda eg: old.gap(eg))
   egs    = egs[:NEAR]
   envy   = eg0]
   for eg in egs[1:]:
     if eg.dominate(envy):
       envy=eg  # the thing that most dominates
   slopes=Eg(xs=zeros(egs[0]))  # local slopes
   for eg in egs:
     step=eg.gap(envy)
     slopes.ys=[(o1 - o2) / step / len(egs)
                for o1, o2 in zip(eg.ys, envy.ys)]
   dist=old.gap(envy)  # how far to push
   mutant.xs=[x if r() > CR else x + FF * (a - x)
                for x, a in zip(old.xs, envy.xs)]
   mutant.ys=[y + slope * dist  # push down slope
               for y, slope in zip(old.ys, slopes.ys)]
   return mutant if mutant.dominate(old) and
                    not stats.same(old, mutant) else old
#else:
#     bests=elite(egs, 0.5)
#     best1=mid(bests)  # thing near mid of non-dominated egs
#     egs=[eg for eg in bests  # closer to heaven than me
#              if best1.gap(eg) < old.gap(best1)]
#     envy=mid(egs).nearest(egs)  # mid of the most heavenly
#
# the de trick incremental domination within the archive
# what about the moea/d trick?
# the surroage trick: eval as few times as possible

# -----
egs=[eval(Eg()) for _ in range(SOME)]
b4=None
while LIVES > 0
  stats=Stats(egs)
  if BATCH
    for a in len(egs):
      egs[a]=mutate(egs[a], egs, stats)
  else:
     egs=[mutate(eg, egs, stats) for eg in egs]
  # here... reevaluate egs
  LIVES -= 1
  if b4:
    if better(stats, b4): LIVES += 1
  b4=Stats(egs)
