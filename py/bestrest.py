import xy
import random

def bestRest(src, pop=30,  lt="<", gt=">"):
  class Eg:
    def __init__(i,x,y): i.n, i.x, i.y = 0, x, y
    def __lt__(i,j)    : return i.n < j.n

  class Num:
    def __init__(i,name): 
      i.lo, i.hi,  i.name = 10**32, -10**32, name
      i.w = i.weight( name[0] )
    def __add__(i,z):
      i.lo = min(i.lo, z)
      i.hi = max(i.hi, z)
    def norm(i,z):
      return (z-i.lo)/(i.hi-i.lo+0.00001)
    def weight(i,s):
      if s == lt: return -1
      if s == gt: return  1
      return 0

  def gt(y0,y1,goal):
    s1, s2, n = 0, 0, len(goals)
    for y in goals:
      a   = y.norm( y0[y.c] )
      b   = y.norm( y1[y.c] )
      s1 -= 10**(y.w * (a-b)/n)
      s2 -= 10**(y.w * (b-a)/n)
    return s1/n < s2/n
  
  def divide(best,lst, goals):
    some = best + random.shuffle(lst)[:pop]
    for eg1 in lst:
      eg1.n = sum([ gt(eg1.y, eg2.y, goals) 
                     for eg2 in some ])
      lst.sort()
      n = len(lst) - pop
      return lst[:n], lst[n:]

  goals, lst, best, rest = [], [], [], []
  x0, y0 = None, None
  for x,y in xy.egs(src):
    if not goals:
      x0, y0 = x,y
      goals  = [ Num(s) for s in enumerate(y) ]
    else:
      [ goal + y1 for y1,goal in zip(y,goals) ]
      lst += [ eg(x,y,goals) ]
      if 0 == len(lst) % pop:
        best,rest = divide(best, lst, goals)
        yield best,rest
  yield divide(best, lst, goals)

if __name__ == "__main__":
  for x in dom("../data/weather.csv"):
      print(x)


