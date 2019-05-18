from xy import xy
import bisect

def dom(src, pop=30, no="?", lt="<", gt=">"):
  class num:
    def __init__(i,c,w): 
      i.lo, i.hi, i.c, i.w = 10**32, -10**32, c, w
    def __add__(i,z):
      if z != ignore:
        i.lo = min(i.lo, z)
        i.hi = max(i.hi, z)
    def norm(i,z):
      return z if z is no else (z-i.lo)/(i.hi-i.lo+0.00001)

  class elite:
     def __init__(i,most=pop,rest): 
       i.all=[]; i.most=pop,i.rest=rest
     def put(i,key,val,rest):
       new = (key,val)
       if len(i.all) < i.most: 
         i.all += [new]
       else:
         n = bisect.bisect_right(i.all,new)
         if n:
           bisect.insort_right(i.all, new)
           _,least = i.all.pop(0)
           i.rest   += [least]
         else:
           i.rest   += [val]

  def nump(z):
     return z != ignore and isinstance(z, (int,float))
 
  def dom(y0,y1):
    s1,s2,n = 0,0, len(goals)
    for y in goals:
      a0,b0 = y0[y.c], y1[y.c]
      a,b   = y.norm(a0), y.norm(b0)
      s1   -= 10**(y.w * (a-b)/n)
      s2   -= 10**(y.w * (b-a)/n)
    return s1/n < s2/n

  def weight(s):
    if s == lt: return -1
    if s == gt: return  1
    return 0

  xs, ys, w, rest = {}, [], [], []
  best = elite(pop,rest)
  for x,y in xy(src):
    if not xs:
      xs = [ num(c, weight(s[0]) for c,s in enumerate(x) ]
      ys = [ num(c, weight(s[0]) for c,s in enumerate(y) ]
      goals = [y for y in ys if y.w != 0]
    else:
      [n + z for z,n in zip(x,xs) if nump(z)]
      [n + z for z,n in zip(y,ys) if nump(z)]
      doms = sum( [doms(y,y1) for _,(_,y1) in best.all ] )
      best.put(doms,(x,y))
