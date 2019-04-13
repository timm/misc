# vim: ts=2 sw=2 sts=2 expandtab:cindent:formatoptions+=cro
#---------1---------2---------3---------4---------5---------

r   = random.random
any = random.choice

def of(lo,hi,x)  : return  lo + x*(hi-lo)
def wrap(lo,hi,x): return  lo + (x - lo) % (hi-lo) 

class o(object):
  def __init__(i, **d): 
    super().init(); i.defaults(); i.__dict__.update(d)
  def __repr__(i): 
    return '%s%s' % (i.__class__.__name__. i.__dict__)
  def defaults(i): pass

class is(o):
  def defaults(i) : pass
  def has(i)      : pass
  def have(i,n)   : return [i.has() for _ in range(n)]
  def ok(i,x)     : True
  def mutate(i,x) : return i.has()
  def mutates(i,x): return i.try(x,i.mutate) 
  def interpolates(i,x): return i.try(x,i.interpolate) 
  def try(i,x,f,n=16)
    while n>0:
      new = f(x)
      if i.ok(new): return new
      n -= 1

class num(is):
  def defaults(i) : i.lo, i.hi = 0, 1
  def has(i)  : return of(i.lo,i.hi,r())
  def ok(i,x) : return i.lo <= x <= i.hi
  def interpola(i,x,y,f=0.5,cr=0.3):
    return 0,False if r() > cr else f*(x - y),True
  def mutate(i,x): return wrap(i.lo,i.hi,x+(i.hi-i.lo)*r())

class oneof(is):
  def defaults(i) : i.range=[True,False]
  def has(i)      : return any(i.range)
  def ok(i,x)     : return x in i.range
  def intraploate(i,x,y,f=0.5,cr=0.3): 
    return x,False if r() > cr else (x if r()<f else y),True
  def mutate(i,x): return i.has()

class bool(one): pass

class triangle(num):
  def defaults(i) : i.lo, i.c, i.hi = 0, 0.5, 1
  def has(i)  : return of(i.lo,i.hi,i.c+(r()-i.c)*r()**0.5)

class norm(num):
  def defaults(i) : i.mu, i.sd = 0, 1
  def has(i)  : return random.normal(i.mu,i.sd)
  def ok(i,x) : return i.mu - 3*i.sd < x < i.mu + 3*i.sd



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
