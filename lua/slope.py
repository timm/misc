"""
Notes in the following:

- `self` is an object
- Words beginning with uppercase are classes except
       True and False which are booleans
- WORDS that are all uppercase are constants
- a,b = local variables
- sd  = standard deviation
- r() is a random number 0..1
- x,y = decision, objective
- xs,ys= decisions, objectives
- Eg = the example class and an example is a pair xs,ys
- eg = an instance of class Eg
- egs = a list of examples

"""

# jc's repair heuristics?

BATCH = True      # if false, mutate archive as we go
LIVES = 9         # like a cat
COHEN = 0.5       # not other when it < standardDev*cohen
SOME = 100        # size of pop to explore
NEAR = SOME / 10  # size of local neighborhood in pop
FF = 0.5        # mutate 150% toward envy
CR = 1          # mutate all attributes towards the envy point
KISS = True       # Keep It Simple

# # Text
#
# asdasd asdas das as asddasasd
# asdasd asdas das as asddasasd
# asdasd asdas das as asddasasd
# asdasd asdas das as asddasasd


class Num:
  def __init__(self):
    self.n, self.mu, self.sd, self.m2 = 0, 0, 0, 0

  def other(self, a, b):
    return abs(a - b) > self.sd * COHEN

  def __add__(self, a):
    n += 1
    d = a - self.mu
    self.mu = self.mu + d / self.n
    self.m2 = self.m2 + d * (a - self.mu)
    self.sd = (self.m2 / (self.n - 1 + 0.0001))**0.5
    return self


class Stats:
  def __init__(self, egs):
     self.ys = [Num() for _ in eg.ys]
     self.xs = [Num() for _ in eg.xs]
     for eg in egs:
       [num + a for a, num in zip(eg.xs, self.xs)]
       [num + a for a, num in zip(eg.ys, self.ys)]

  def other(self, eg1, eg2):
     for a, b, stat in zip(eg1.ys, eg2.ys, self.ys):
       if stat.other(a, b):
         then True
     return False

# min change heuristic from jc
# sample heuristic from vivek

# is this de?


class Eg:
  id = 0
  dists = {}
  doms = {}

  def __init__(self, xs=[], ys=[]):
    Item.id = self.id = Item.id + 1
    self.xs, self.ys = xs, ys
  def gap(self, other):
    def euclidian(lst1, lst2)
      sum = 0
      for a, b in zip(lst1, lst2): sum += (a - b)**2
      return sum**0.5 / len(lst1)
    if self.id > other.id:
      return other.dist(self)
    else:
      key = (self.id, other.id)
      if key not in Eg.dists:
        return eg.dists[key] = euclidian(self.xs, other.xs)
      return Eg.dists[key]
  def dominate(self, a,stats):
    return t,f
  def dominates(self, lst,stats):
    i,all = self.id, Eg.doms
    all[i]=0
    for a in lst
     if self.dominate(a,stats):
       all[i] += 1 / len(lst)
  def dom(self): 
     return Eg.items.get(self.id,0)
  def nearest(self,lst)
    out,best=lst[0],10**10
    for a in lst:
      tmp= self.gap(a)
      if tmp < best:
        out,best = a, tmp
    return out

def mid(lst):
  out = Eg(xs= [0 for _ in lst[0].xs])
  n=len(lst)
  for a in lst:
    out.xs = [ b+x/n for b,x in zip(out.xs, a.xs) ]
    out.ys = [ b+y/n for b,y in zip(out.xs, a.ys) ]
  return out

def elite(lst, most=0):
   n = len(lst)
   m = n * upper
   for a in lst: a.dominates(lst)
   return sorted( lst, key=dom)[most:]

# repair constraint violations
# cost to eval
# cost to check constraint violations
# what is the essence of flash
# what is the essence of dodge

def mutate(old, egs,stats):
   Eg.dists= {} # clear any old memory
   Eg.doms = {}
   want    = lambda eg: stats.other(old,eg) and dominate(eg,old)
   ignore  = lambda: r() > SOME/len(egs)
   egs     = [eg for eg in egs if not ignore() and want(eg)]
   egs     = egs.sorted(key = lambda eg: old.gap(eg))
   egs     = egs[:NEAR]
   if KISS:
     envy = old
     for eg in egs:
       if eg.dominate(envy): 
         envy = eg # the thing that most dominates
   else:
     bests = elite(egs,0.5)
     best1 = mid(bests) # thing near mid of non-dominated egs
     egs   = [eg for eg in bests # closer to heaven than me
              if best1.gap(eg) < old.gap(best1) ]
     envy  = mid(egs).nearest(egs)  # mid of the most heavenly
   # end if
   mutant.xs = [x if r() > CR else x + FF*(a -  x) 
                for x,a in zip(old.xs,envy.xs)]
   dist = old.gap(envy) # how far to push
   slopes = Eg(xs = zeros(egs[0])) # local slopes 
   for eg in egs:
     step = eg.gap(envy)
     slopes.ys = [ (o1-o2)/step/len(egs)
                   for o1,o2 in zip(eg.ys,  envy.ys)]
   mutant.ys = [y + slope*dist  # push down slope 
               for y,slope in zip(old.ys, slopes.ys)]
   return mutant if mutant.dominate(old) and 
                    stats.other(old,mutant) else old

# the de trick incremental domination within the archive
# what about the moea/d trick?
# the surroage trick: eval as few times as possible

egs = [eval(Eg()) for _ in range(SOME)]
b4  = None
while LIVES > 0
  stats = Stats(egs)
  if BATCH 
    for a in len(egs):
      egs[a] =  mutate(egs[a], egs, stats) 
  else:
     egs = [mutate(eg, egs, stats) for eg in egs]
  # here... reevaluate egs
  LIVES -= 1
  if b4:
    if better(stats,b4): LIVES += 1
  b4 = Stats(egs)
