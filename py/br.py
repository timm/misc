import xy  
from random import choice as any

def br(src, pop=30, lt="<", gt=">", gen=128):
  class Eg:
    def __init__(i,x,y): i.x, i.y, i.n, i.best = x,y,0,False
    def __lt__(i,j): return i.n > j.n
    def __repr__(i): 
      return 'Eg(x=%s, y=%s, n=%s)' % (i.x, i.y, i.n)

  class HiLo:
    def __init__(i,name="txt"): 
      i.lo, i.hi,  i.name = 10**32, -10**32, name
      s = name[0]
      if   s == lt: i.w = -1
      elif s == gt: i.w =  1
      else 
        assert False , "header type [%s] not '<' or '>'" % s
    def __add__(i,z): i.lo, i.hi = min(i.lo,z), max(i.hi,z)
    def norm(i,z)   : return (z-i.lo)/(i.hi-i.lo+0.00001)

  def gt(y0,y1, hilos):
    s1, s2, n = 0, 0, len(hilos)
    for a,b,hilo in zip(y0,y1,hilos):
      a   = hilo.norm( a )
      b   = hilo.norm( b )
      s1 -= 10**(hilo.w * (a-b)/n)
      s2 -= 10**(hilo.w * (b-a)/n)
    return s1/n < s2/n

  def rank(best, egs, hilos):
    for eg1 in egs:
      eg1.best = False
      eg1.n    = sum([gt(eg1.y, eg2.y, hilos) for eg2 in best]) 
    egs.sort()
    best = egs[:pop]
    for eg in best: eg.best=True
    return best, egs

  hilos, latest, best, egs = [], [], [], []
  for x,y in xy.egs(src):
    if not hilos:
      hilos  = [ HiLo(s) for s in y ]
      yield  (x,y)
    else:
      for y1,hilo in zip(y,hilos): hilo + y1
      eg      = Eg(x,y)
      egs    += [ eg ]
      latest += [ eg ]
      if 0 == len(egs) % gen:
        best,egs = rank(best or [any(egs) for _ in range(pop)]
                        egs, hilos)
        for eg in latest: yield eg
        latest = []
  # finale
  rank(best, egs, hilos)
  for eg in latest: yield eg

if __name__ == "__main__":
  def report(what,y,ls):
    if ls:
      egs=None
      for l in ls:
        egs = all or [[] for _ in y]
        [ l0.append(z)  for z,l0 in zip(l.y,egs) ]
      print(what, [int(sum(l)/len(l)) for l in egs])
    else:
      print(what,[])
  for x,y,bs,rs in br("../data/auto93.csv"):  
    print(len(bs), len(rs))
    print("")
    report("best", y, bs)
    report("rest", y, rs)
