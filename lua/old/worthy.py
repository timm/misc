# vim: ts=2 sw=2 sts=2 expandtab:cindent:formatoptions+=cro
#---------1---------2---------3---------4---------5---------

r   = random.random
any = random.choice

def of(lo,hi,x)  : return  lo + x*(hi-lo)

#---------1---------2---------3---------4---------5---------
class o(object):
  def __init__(i, **d): 
    super().init(); i.defaults(); i.__dict__.update(d)
  def __repr__(i): 
    return '%s%s' % (i.__class__.__name__. i.__dict__)
  def defaults(i): pass

#---------1---------2---------3---------4---------5---------
class is(o):
  def defaults(i)    : pass
  def ok(i,x)        : True
  def has(i)         : pass
  def have(i,n)      : return [i.has() for _ in range(n)]
  def tap(i,x)       : pass
  def kick(i,x,a,b,c): pass
  def taps(i,x)      : return i.vary(x,lambda:i.tap(x)) 
  def kicks(i,a,b,c) : return i.vary(x,lambda:i.kick(a,b,c)) 
  def vary(i,old,f,n=16):
    while n>0:
      n -= 1
      new = f()
      if new ~= old and i.ok(new): return new
    assert False,"cannot find a new value"

#---------1---------2---------3---------4---------5---------
class oneof(is):
  def defaults(i) : i.range=[True,False]
  def ok(i,x)     : return x in i.range
  def has(i)      : return any(i.range)
  def kick(i,x)   : return i.has()
  def tap(i,x,a,b,c,f=0.5,cr=0.3): 
    return x if r() > cr else i.has()

class bool(oneof): pass

#---------1---------2---------3---------4---------5---------
class num(is):
  def defaults(i) : i.lo, i.hi = 0, 1
  def ok(i,x)     : return i.lo <= x <= i.hi
  def has(i)      : return of(i.lo,i.hi,r())
  def wrap(i,x)   : return i.lo + (x - i.lo) % (i.hi-i.lo) 
  def kick(i,x)   : return i.wrap(x + (i.hi-i.lo)*r())
  def tap(i,a,b,c,f=0.5,cr=0.3):
    return a+ (0 if r() > cr else f*(b - c))

class triangle(num):
  def defaults(i) : i.lo, i.c, i.hi = 0, 0.5, 1
  def has(i)      : return of(i.lo,i.hi,
                              i.c+(r()-i.c)*r()**0.5)

class norm(num):
  def defaults(i) : i.mu, i.sd = 0, 1
  def ok(i,x)     : return i.mu - 3*i.sd < x < i.mu + 3*i.sd
  def has(i)      : return random.gauss(i.mu,i.sd)



#---------1---------2---------3---------4---------5---------
def worthy():
  more =  1
  less = -1

    def weight()             : return any([more,less])
  def num(lo=0, hi=1)      : return lo+r()*(hi - lo)
  def sym(lst=[True,False]): return any(lst)

  def thing():
   return o(x= o( nums= have(n=5,eg=num),
                  syms= have(n=2,eg=sym)),
            y=  o( nums= have(n=3,eg=num),
                   ws  = have(n=2,eg=weight)))

  def eg(d) : 
    if type(d) == "have": return d()
    if type(d) != "o"  : return d
    return {(k:example(v) for k,v in d.items())}

  def egs(n,about): 
    return have(n=n,eg= lambda: eg(about))()

  about = thing()
  p=0
  pop=[eg(n,about)]


"""
my = {all  = 100
     ,some = 100
     ,k    = 5
     ,x    = {num=10, sym=0}
     ,y    = {more=3, less=2, klass=0} 
     }

#---------#---------#---------#---------#---------#---------
sub(x,y)   = x - y
change(x,y) = nil if x==y else y
zero()     = 0
id         = 0

dec(item) = item.sym  ++ item.num
obj(item) = item.more ++ item.less

dist(item1,item2,what=dec) = distance between what

cluster(lst,what=dec) = hiearchical cluster of lst using
                        dist(what) with |leaf| >= the.k

nearest(k,item,lst,what=dec) = all other items that are k closest
                          to item using dist(what)

ok(valids, old, new) = repair(valids,old,new) if bad(new) else new 

repair(valids, old, new) = 
   for valid in valids
     do binary chops new to valid.x looking for something close to new that is not bad
         if found return it
  return old

#---------#---------#---------#---------#---------#---------
num(n=1) = [random()          for range(n)]
sym(n=1) = [any('randomStuf') for range(n)]
item()   = {x = {num   = num( my.x.num  )
                ,sym   = sym( my.x.sym  )}
           ,y = {klass = sym( my.y.klass )
                ,more  = num( my.y.more  )
                ,less  = num( my.y.less  )}
items(n) = [item() for range( n )]
           or from Z3

#---------#---------#---------#---------#---------#---------
delta(a,b) = 
   {id    = id++
    ,x    = a
    ,y    = b
    ,sym  = map(change, a.x.sym, b.x.sym)
    ,num  = map(sub, a.x.num, b.x.num)
    ,more = map(sub, a.y.more, b.y.more)
    ,less = map(sub, a.y.less, b.y.less)
    }
i=0
pop[i]= items( my.all ) # n items

change langaiuge to soverample and priunce. suroage. contrast . simialrity. diversirty . evaluationspeed . theoreem somethimes

verify oepration: if discrete explore the CNF. inf contonusous, uas the surroage if it improves things
     if vertification fails, got to repair.

:PLAN 
  # learn candidate mutants
  some  = shuffle(pop[i])[1:the.some]
  tree  = cluster( map(delta, some, some), what=dec)
  plans = all deltas in all leaves of tree
            with at least one less,more more than 1*sd of all mean less,more
            with at least one mean less,more going in right direction

# simpler: dont bother clsutering. jsut do deltas between anything at all

  # score candidate mutants 
  for plan in plans
    local = nearest(the.k, plan, plans, what=dec)
    plan.predict.more = mean(more in local)
    plan.predict.less = mean(less in local)

:MUTAT
  # mutate and score with surrogate
  pop[i+1] = []
  for old in pop[i]
    best = old
    local = nearest(the.k, old, plans, what=dec) # sortet closest to furthest
    for plan in local
      new = old + plan.sym + plan.num + plan.more + plan.less
      new = ok(local, old, new)
      if dominates(new, best)
        best = new
    pop[i+1] += [best]

:LOOP
  i++
  if we have a model, then
      evaluate(pop[i]) 
      goto :PLAN
  else
      goto :MUTATE


Input:

MAKE() ==> produces a valid example {decs = [number]+}
objs=SCORE(eg) ==> less=[number]*, more=[number]*
OK(x) ==> TRUE, FALSEtests validity of decisions
P= 100 (say)
D=1000 (say)
diff(eg1,eg2) ==> {improvement= nil
                   from=eg1, to=eg2, 
                   dist= {objs=distance(eg1,eg2.what=objs)
                          decs=distance(eg1,eg2,what=decs)}
                   delta = {objs=[number]+, objs=[number]+}

:SAMPLE
Pop=  [SCORE((MAKE()) for _ in range(P}]
Deltas = [diff(any(Pop), any(Pop)) for _in range(D)]

for d in Deltas
   Collect statistics on mean and standard deviation of all dist.objs, dist.decs  and delta of all 

###################################
:SURROGATES

lives = 9   # like a cat
cohen = 0.5 # things less than standardDev*cohen are not really different
some = 100  # size of pop to explore
near = n/10 # size of local neighborhood in pop
f=0.5       # mutate 150% toward envy
cr=1        # mutate all attributes towards the envy point
kiss=True   # Keep It Simple

class Num:
  def __init__(self):
    self.n,self.mu, self.sd, self.m2 = 0,0,0,0
  def small(self,a,b):
    return abs(a-b) <= self.sd *d
  def __iadd__(self,z):
    n      += 1
		d       = z - self.mu
  	self.mu = self.mu + d/self.n
    self.m2 = self.m2 + d*(z - self.mu)
    self.sd = (self.m2/(self.n - 1 + 0.0001))s**0.5 
    return self

class Stats:
  def __init__(self, egs):
     self.ys= [ Num() for _ in eg.ys]
     self.xs= [ Num() for _ in eg.xs]
     [self += eg for eg in egs]
  def __iadd__(self,new):
     [num + z for z,num in zip(new.xs, self.xs]
     [num + z for z,num in zip(new.ys, self.ys]
	   return self
  def different(self,this,that):
     for a,b,stat in zip(this.ys,that.ys,self.ys):
       if abs(b-a) > stat.sd*cohen:
         then True
     return False

# min change heuristic from jc
# sample heuristic from vivek

# is this de?

xs: lambda a: a.xs
ys: lambda a: a.xs
dom:  lambda a: a.dom()

class Eg:
  id=0
  dists={}
  doms={}
  def __init__(self, xs=[],ys=[]):
    Item.id = self.id = Item.id + 1
    self.xs, self.ys = xs,ys
  def distance(self,other,using=xs):
    if self.id > other.id: 
      return other.dist(self)
    else:
      key = (self.id,other.id)
      if key not in Eg.dists:
        return eg.dists[key] = euclidian(using(self), using(other))
      return Eg.dists[key]
   def dominates(self, lst,stats):
     i,all = self.id, Eg.doms
     all[i]=0
     for z in lst
      if self.dominates(z,stats):
         all[i] += 1 / len(lst)
   def dom(self): return Eg.items.get(self.id,0)
   def nearest(self,lst)
     out,best=lst[0],10**10
     for z in lst:
       tmp= self.distance(z)
       if tmp < best:
         out,best = z, tmp
     return out
   def dominate(self, z,stats):
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
   for z in lst: z.dominates(lst)
   return sorted( lst, key=dom)[most:]

egs = some initial evaluations
b4  = None
lives 
while lives > 0
  lives -= 1 
  stats = Stats(egs)
  egs = [ mutate(a, pos, stats) for a in egs ]
  if b4:
    if better(egs,b4): lives += 1
  b4 = egs

def mutate(old, egs,stats):
   # clear any old memory
   Eg.dists={}
   Eg.doms={}
   ignore = lambda: r() > some/len(egs)

   #interesting things clearly dominate old
   def inteseting(eg):
     return stats.different(old,eg) and dominate(eg,old)

   egs = [eg for eg in egs if not ignore() and interesting(eg)]

   #interesting things are nearby
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
     egs = [eg for eg in best if distance(eg,heaven) < distance(old,heaven) ]
  
     # so i envy the thing nearest the centroid of the intereting itesm
     envy  = centroid(egs).nearest(egs)

   #-----------------------------------------------------
   # now i have the egs pruned and an i have an envy point
   # so can mutate f-th of old towards envy
   mutant.xs = [x if r() > cr else x + f*(z -  x) for
                x,z in zip(old.xs,envy.xs)]

   #-----------
   # and now i must use interpolation to guess what happens to the yectives

   # step1 determine how far i have to push
   dist = distance(old, envy, using="xs")

   # step2: find the local slopes
   slopes = eg0(egs[0])
   for eg in egs
     step = eg.distance(envy)
     slopes.ys =  [ (o1-o2)/step/len(egs)
                      for o1,o2 in zip(eg.ys,  envy.ys)]

   # step3: push the yectives down those slopes
   mutant.ys = [y + slope*dist for y,slope for zip(old.ys, slopes.ys)]
   return mutant if mutant.dominates(old) and stats.different(old,mutant) else old

"""
