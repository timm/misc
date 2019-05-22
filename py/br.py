import xy  
import random

class Num:
  lt = "<"
  gt = ">"
  def __init__(i,name="<txt"): 
    i.n, i.mu, i.lo, i.hi, i.name = 0,0,10**32,-10**32,name
    s = name[0]
    if   s == Num.lt: i.w = -1
    elif s == Num.gt: i.w =  1
    else:
      assert False, "header [%s] not '<' or '>'" % s
  def __add__(i,z): 
    i.n  += 1
    i.mu += (z - i.mu ) / i.n
    i.lo, i.hi = min(i.lo,z), max(i.hi,z)
  def norm(i,z): 
    return (z - i.lo)/(i.hi - i.lo + 0.00001)

def br(src, pop=32, gen=256):
  class Eg:
    def __init__(i,x,y): 
      i.x, i.y, i.n, i.klass = x, y, 0, False
    def __lt__(i,j): 
      return i.n > j.n
    def __repr__(i): 
      return 'Eg(x=%s y=%s n=%s)'%(i.x,i.y,i.n)

  class Frontier:
    def __init__(i,y): 
      i.best, i.egs, i.nums = [],[],[Num(s) for s in y]
    def update(i, latest):
      i.egs += latest
      for eg in latest:
        for y,num in zip(eg.y, i.nums): 
          num + y  # update the known lows and highs
    def gt(i,y0,y1):
      s1, s2, n = 0, 0, len(i.nums)
      for a,b,num in zip(y0,y1,i.nums):
        a   = num.norm( a )
        b   = num.norm( b )
        s1 -= 10**(num.w * (a-b)/n)
        s2 -= 10**(num.w * (b-a)/n)
      return s1/n < s2/n #btw, in Python, True,False=1,0
    def bests(i):
       if not i.best:
         random.shuffle(i.egs)
         i.best = i.egs[:pop] # best initializes to random
       return i.best
    def klass(i, new):
       i.update(new) 
       for eg in i.egs: 
         eg.klass = False  # reset. nothing is best.
         eg.n     = sum([i.gt(eg.y, b.y) for b in i.bests()]) 
       i.egs.sort()
       i.egs  = i.egs[:gen] # stop i.egs from growing too big
       i.best = i.egs[:pop] # select the best
       for eg in i.best:    # classify all that is best
         eg.klass = True
       return new

  new = []
  for n,(x,y) in enumerate(xy.egs(src)):
    if n > 0: # usual case
      new += [ Eg(x,y) ]
      if n % gen == 0:
        for eg in front.klass(new): 
          yield eg
        new = []
    else:  # initial case, one time only
      front = Frontier(y)
      yield (x,y)
  for eg in front.klass(new): 
    yield eg

if __name__ == "__main__":
  for n,eg in enumerate(br("../data/auto93.csv")):
    if n > 0:
      [num + y for num,y in zip(good if eg.klass else bad,eg.y)]
    else:
      print(n)
      bad  = [Num(s) for s in eg[1]]
      good = [Num(s) for s in eg[1]]
  print("good", [int(num.mu) for  num in good])
  print("bad ", [int(num.mu) for  num in bad])
