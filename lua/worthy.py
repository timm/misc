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
