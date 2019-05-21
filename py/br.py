import xy  
import random

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

  class Frontier:
    def __init__(i,y): 
      i.best, i.egs, i.hilos  = None, [],[HiLo(s) for s in y]
    def update(i, latest):
      i.egs += latest
      for eg1 in latest:
        for y,hilo in zip(eg.y,i.hilos): hilo + y1
    def gt(i,y0,y):
      s1, s2, n = 0, 0, len(i.hilos)
      for a,b,hilo in zip(y0,y1,i.hilos):
        a   = hilo.norm( a )
        b   = hilo.norm( b )
        s1 -= 10**(hilo.w * (a-b)/n)
        s2 -= 10**(hilo.w * (b-a)/n)
      return s1/n < s2/n
    def bests(i):
       i.best = i.best or [
                random.choice(i.egs) for _ in range(pop)]
       return i.best
    def classify(i, latest):
       i.update(latest) 
       for eg in i.egs:
         eg.best = False
         eg.n    = sum([i.gt(eg.y, b.y) for b in i.bests()]) 
       i.egs.sort()
       i.egs  = i.egs[:gen]
       i.best = i.egs[:pop]
       for eg in i.best: eg.best=True
       return latest
      
  latest = []
  for n,(x,y) in enumerate(xy.egs(src)):
    if n == 1
      frontier = Frontier(i,y)
      yield (x,y)
    else:
      latest += [ Eg(x,y) ]
      if n % gen == 0:
        for eg in frontier.classify(latest): 
          yield eg
        latest = []
  for eg in frontier.classify(latest): 
    yield eg

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
