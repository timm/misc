import xy, random

def br(src, pop=30, lt="<", gt=">", gen=128):
  goals, lst, best, rest, x0, y0 = [],[],[],[],None,None

  class Eg:
    def __init__(i,x,y): i.n, i.x, i.y = 0, x, y
    def __lt__(i,j)    : return i.n > j.n
    def __repr__(i)    : 
      return 'Eg(x=%s, y=%s, n=%s)' % (i.x, i.y, i.n)
  class Num:
    def __init__(i,name="txt"): 
      i.lo, i.hi,  i.name = 10**32, -10**32, name
      i.w = i.weight( name[0] )
    def __add__(i,z): i.lo, i.hi = min(i.lo,z), max(i.hi,z)
    def norm(i,z)   : return (z-i.lo)/(i.hi-i.lo+0.00001)
    def weight(i,s):
      if s == lt: return -1
      if s == gt: return  1
      return 0

  def gt(y0,y1):
    s1, s2, n = 0, 0, len(goals)
    for a,b,goal in zip(y0,y1,goals):
      a   = goal.norm( a )
      b   = goal.norm( b )
      s1 -= 10**(goal.w * (a-b)/n)
      s2 -= 10**(goal.w * (b-a)/n)
    return s1/n < s2/n
  def divide(best,lst):
    for eg1 in lst:
      eg1.n = sum([ gt(eg1.y, eg2.y) for eg2 in best ]) 
      print(eg1.n)
    lst.sort()
    return lst[:pop], lst[pop:]

  r=0
  for x,y in xy.egs(src):
    if not goals:
      x0, y0 = x,y
      goals  = [ Num(s) for s in y ]
    else:
      [ goal + y1 for y1,goal in zip(y,goals) ]
      lst += [ Eg(x,y) ]
      if 0 == len(lst) % gen:
        r += 1
        best, rest = divide(best, lst)
        if r > 1:
          yield x0,y0, best,rest

if __name__ == "__main__":
  def report(what,y,ls):
    if ls:
      all=None
      for l in ls:
        all = all or [[] for _ in y]
        [ l0.append(z)  for z,l0 in zip(l.y,all) ]
      print(what, [int(sum(l)/len(l)) for l in all])
    else:
      print(what,[])
  for x,y,bs,rs in br("../data/auto93.csv"):  
    print(len(bs), len(rs))
    print("")
    report("best", y, bs)
    report("rest", y, rs)
