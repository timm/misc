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

:SURROGATES

d=0.5
jump=2
n = 100
near = n/10
f=0.5
cr=0.5

class Num:
  def __init__(self):
    self.n,self.mu, self.sd, self.m2 = 0,0,0,0
  def small(self,x,y):
    return abs(x-y) <= self.sd *d
  def __iadd__(self,x):
    n      += 1
		d       = x - self.mu
  	self.mu = self.mu + d/self.n
    self.m2 = self.m2 + d*(x - self.mu)
    self.sd = (self.m2/(self.n - 1 + 0.0001))s**0.5 
    return sekf

class Stats:
  def __init__(self, example):
     self.objs= [ Num() for _ in example.objs]
     self.decs= [ Num() for _ in example.decs]
  self __iadd__(self,new):
     [num + z for x,num in zip(new.decs, self.decs]
     [num + z for x,num in zip(new.objs, self.objs]
	   return self


def different(xs,ys,nums):
	for num,x,y in zip(nums,xs,ys)
    if not small(x,y): return True
  return False

for x in pop
  x= mutate(x, any n of pop)

# cachec all the distance calcs
# min change heuristic from jc
# what if best is brittle (better to avoid worst?)
# find tose in best half. work toward theravegat cirection

# is this de?

decs: lambda z: z.decs
objs: lambda z: z.decs
dom:  lambda z: z.dom()

class Eg:
  id=0
  dists={}
  doms={}
  def __init__(self, decs=[],objs=[]):
    Item.id = self.id = Item.id + 1
    self.decs, self.objs = decs,objs
  def distance(self,other,using=decs):
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
     for y in lst
      if x.dominates(y,stats):
         all[i] += 1 / len(lst)
   def dom(self): return Eg.items.get(self.id,0)
   def nearest(self,lst)
     out,best=lst[0],10**10
     for x in lst:
       tmp= self.distance(x)
       if tmp < best:
         out,best = x, tmp
     return out
   def dominate(self, x,stats):
     return t,f

def centroid(lst):
  out=clone(lst[0])
  n=len(lst)
  for x in lst:
    out.decs = [z+dec/n for z,dec in zip(out.decs, x.decs) ]
    out.objs = [z+obj/n for z,obj in zip(out.decs, x.objs) ]
  return out
  
def euclidian(xs,ys):
  sum=0
  for x,y in zip(xs,ys): sum += (x-y)**2
  return sum**0.5/ len(xs)

def clone(eg):
   return Eg( decs = [0 for _ in eg.decs], 
              objs = [0 for _ in eg.objs])

def elite(lst, most=0):
   n = len(lst)
   m = n * upper
   for y in lst: y.dominates(lst)
   return sorted( lst, key=dom)[most:]

def mutate(x, ys,stats, some=100):
   # clear any old memory
   Eg.dists={}
   Eg.doms={}
   ignore = lambda: r() > some/len(ys)

   #interesting things clearly dominate x
   def inteseting(y):
     return different(y,x,stats,objs) and dominate(y,x)

   ys = [y for y in ys if not ignore() and interesting(y)]

   #interesting things are nearly
   ys = ys.sorted(key = lambda y: x.distance(y))
   ys = ys[:near]

   if reckless::
     # I most envy the thing which most domiantes 
     # I most interested in things closer than me to what i envy
     envy = elite(ys)[-1]
     ys = [y for y in ys if distance(y,envy) < distance(x,envy) ]
 
   else:
     # heaven is en imaginaty point  nearest the centroid of the non-dominated examples
     best = elite(ys,0.5)
     heaven= centroid( best )
     
     # i'm most interested in the things closer to heaven than me
     ys = [y for y in best if distance(y,heaven) < distance(x,heaven) ]
  
     # so i envy the thing nearest the centroid of the intereting itesm
     envy  = centroid(ys).nearest(ys)

   #-----------------------------------------------------
   # now i have the ys pruned and an i have an envy point
   # so can mutate cr-th of x towards envy
   x.decs = [dec if r() > cr else dec + jump*(z -  dec)*f for
                  dec,z in zip(x.decs,envy.decs)]

   #-----------
   # and now i must use interpolation to guess what happens to the objectives

   # step1 determine how far i have to push
   dist = distance(x, envy, using="decs")

   # step2: find the local slopes
   deltas = clone(ys[0])
   for y in ys
     step = y.distance(envy)
     deltas.objs =  [ (o1-o2)/step/len(ys)
                      for o1,o2 in zip(y.objs,  envy.objs)]

   # step3: push the objectives down those slopes
   x.objs = [obj + jump*delta*dist for obj,delta for zip(x.objs, deltas)]
   return x

"""
