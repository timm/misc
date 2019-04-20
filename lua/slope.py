"""
Notes
- in the following
    self is an object
    a,b = local variables
    sd  = standard deviation
    r() is a random number 0..1
    x,y = decision, objective
    xs,ys= decisions, objectives
    Eg = the example class and an example is a pair xs,ys
    eg = an instance of class Eg
    egs = a list of examples
    eg0() is a function that returns an example with all zeroes for decisions and objectives
"""

# i seek intuitions controlled by constants that can be adjusted

# jc's repair heuristics?

lives = 9   # like a cat
# things less than standardDev*cohen are not really different
cohen = 0.5
some = 100  # size of pop to explore
near = n / 10  # size of local neighborhood in pop
f = 0.5       # mutate 150% toward envy
cr = 1        # mutate all attributes towards the envy point
kiss = True   # Keep It Simple


class Num:
  def __init__(self):
    self.n, self.mu, self.sd, self.m2 = 0, 0, 0, 0

  def small(self, a, b):
    return abs(a - b) <= self.sd * d

  def __iadd__(self, a):
    n += 1
		d = a - self.mu
  	self.mu = self.mu + d/self.n
    self.m2 = self.m2 + d*(a - self.mu)
    self.sd = (self.m2/(self.n - 1 + 0.0001))s**0.5 
    return self

class Stats:
  def __init__(self, egs):
     self.ys= [ Num() for _ in eg.ys]
     self.xs= [ Num() for _ in eg.xs]
     for eg in egs:
       [num + a for a,num in zip(eg.xs, self.xs]
       [num + a for a,num in zip(eg.ys, self.ys]
  def different(self,eg1,eg2):
     for a,b,stat in zip(eg1.ys,eg2.ys,self.ys):
       if abs(b-a) > stat.sd*cohen:
         then True
     return False

# min change heuristic from jc
# sample heuristic from vivek

# is this de?

class Eg:
  id=0
  dists={}
  doms={}

  def __init__(self, xs=[],ys=[]):
    Item.id = self.id = Item.id + 1
    self.xs, self.ys = xs,ys

  def distance(self,other):
    if self.id > other.id: 
      return other.dist(self)
    else:
      key = (self.id,other.id)
      if key not in Eg.dists:
        return eg.dists[key] = euclidian(self.xs, other.xs)
      return Eg.dists[key]

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
       tmp= self.distance(a)
       if tmp < best:
         out,best = a, tmp
     return out

   def dominate(self, a,stats):
     return t,f

def centroid(lst):
  out=eg0(lst[0])
  n=len(lst)
  for a in lst:
    out.xs = [b+x/n for b,x in zip(out.xs, a.xs) ]
    out.ys = [b+y/n for b,y in zip(out.xs, a.ys) ]
  return out
  
def euclidian(lst1,lst2)
  sum=0
  for a,b in zip(lst,lst2): sum += (a-b)**2
  return sum**0.5/ len(lst1)

def eg0(eg):
   return Eg( xs = [0 for _ in eg.xs], 
              ys = [0 for _ in eg.ys])

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
   # clear any old memory
   Eg.dists={}
   Eg.doms={}
   ignore = lambda: r() > some/len(egs)

   # interesting things clearly dominate old
   def inteseting(eg):
     return stats.different(old,eg) and dominate(eg,old)

   egs = [eg for eg in egs if not ignore() and interesting(eg)]

   # interesting things are nearby
   egs = egs.sorted(key = lambda eg: old.distance(eg))
   egs = egs[:near]

   if kiss:
     # I most envy the thing which most domiantes 
     envy = old
     for eg in egs:
       if eg.dominate(envy): 
         envy = eg
   else:
     # heaven is en imaginary point  nearest the centroid of the non-dominated egs
     best = elite(egs,0.5)
     heaven= centroid( best )
     
     # i'm most interested in the things closer to heaven than me
     egs = [eg for eg in best if heaven.distance(eg) < old.distance(heaven) ]
  
     # so i envy the thing nearest the centroid of the intereting itesm
     envy  = centroid(egs).nearest(egs)

   # -----------------------------------------------------
   # now i have the egs pruned and an i have an envy point
   # so can mutate f-th of old towards envy
   mutant.xs = [x if r() > cr else x + f*(a -  x) for
                x,a in zip(old.xs,envy.xs)]

   # -----------
   # and now i must use interpolation to guess what happens to the yectives

   # step1 determine how far i have to push
   dist = old.distance(envy)

   # step2: find the local slopes
   slopes = eg0(egs[0])
   for eg in egs
     step = eg.distance(envy)
     slopes.ys =  [ (o1-o2)/step/len(egs)
                      for o1,o2 in zip(eg.ys,  envy.ys)]

   # step3: push the yectives down those slopes
   mutant.ys = [y + slope*dist for y,slope for zip(old.ys, slopes.ys)]
   return mutant if mutant.dominates(old) and stats.different(old,mutant) else old

## the de trick incremental domination within the archive
## what about the moea/d trick?
## the surroage trick: eval as few times as possible

egs = some initial evaluations
b4  = None
while lives > 0
  stats = Stats(egs)
  for a in len(egs):
    egs[a] =  mutate(egs[a], egs, stats) 
  ## here... reevaluate egs
  lives -= 1
  if b4:
    if better(stats,b4): lives += 1
  b4 = Stats(egs)
