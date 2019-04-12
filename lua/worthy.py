# vim: ts=2 sw=2 sts=2 expandtab:cindent:formatoptions+=cro
#---------1---------2---------3---------4---------5---------
r= random.random

class o(object):
  def __init__(i, **d): 
    super().init(); i.uses(); i.__dict__.update(d)
  def __repr__(i): 
    return '%s%s' % (i.__class__.__name__. i.__dict__)
  def uses(i): pass

class is(o):
  def __call__(i) : return some(i,i.n)
  def any(i,lst)  : return random.choose(lst)
  def has(i)      : pass
  def have(i,n)   : return [i.has() for _ in range(n)]
  def ok(i,x)     : True
  def uses(i)     : i.n = 1

class num(is):
  def has(i)  : return i.lo + r()*(i.hi - i.lo)
  def uses(i) : i.lo, i.hi = 0, 1
  def ok(i,x) : return i.lo <= x <= i.hi

class triangle(num):
  def has(i)     : return i.lo + (i.hi - i.lo) * (
                                  i.s(i.mode, r(), r()))
  def uses(i)    : i.lo, i.mode, i.hi = 0, 0.5, 1
  def s(i,c,u,v) : 
    """"http://www.sciencedirect.com/
        science/article/pii/S0895717708002665"""
    return c + (u-c)*v**0.5

class norm(is):
  def has(i)  : return random.normal(i.mu,i.sd)
  def ok(i,x) : return i.mu - 3*i.sd < x < i.mu + 3*i.sd
  def uses(i) : i.mu, i.sd = 0, 1

class oneof(is):
  def has(i) : return i.any(i.range)
  def uses() : i.range=[True,False]

class bool(one): pass


def worthy():
  r    = random.random
  any  = random.choice
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
