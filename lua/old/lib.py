def d2l(d):
  return [(k,d[k]) for k in sorted(list(d.items)) 
          if k[0] != The.PRIVATE]

class Base(object):
   def __repr__(i):  
     return i.__class__.__name__ + str(da 2l(i.dict))
   def seeBelow(i):  assert 0, "implemented by subclass"

class Stat(Base):
  def same(i, a, b) : i.seeBelow() # true if a,b's diff is more than trivial
  def __eq__(i, j)  : i.seeBelow() # statistically indistinguisable
  def gap(i, a, b)  : i.seeBelow() # distance of 2 things
  def __add__(i, x) : i.n += 1; self.put(x); return i
  def put(i, a)     : i.seeBelow() # add one item to this sample
  def get(i, a)     : i.seeBelow() # add
  @classmethod
  def ako(cls, pat):
    me, no = cls.__name__, r"Stat(s)?"
    if re.match(pat, me) and not re.match(no, me): return cls
    for sub in cls.__subclasses__(): return sub.ako(pat)

def uniform(i): return i.lo + r()*(i.hi - i,lo)
def normal(i): return random.gauss(i.mu, i.sd)
def triangle(i):
    u, v, c = r(), r(), (i.mu - i.lo)/(i.hi - i.lo+0.00001)
    return i.lo + (i.hi - i.lo)*(c+(u-c)*v*0.5)

r = random.random
seed = random.seed
any  = random.choice

class Some(Stat):
  "Keep, at most, 'size' things."
  def __init__(i, size=The.SOME):
    i.max = size or The.SOME
    i.n, i.all = 0, []
  def get(i): return any(i.all)
  def put(i,a):
    i.n += 1
    now  = len(i.all)
    if now < i.max:
      i.all += [a]
    elif r() <= now/i.n:
      i.all[ int(r() * now) ]= a
  def __eq__(i,j): # cliffs delta
    def runs(lst):
        for j, two in enumerate(lst):
            if j == 0     : one, i = two, 0
            if one != two : yield j - i, one; i = j
            one = two
        yield j - i + 1, two 
    lst1,lst2 = i.all, j.all
    size = True
    m, n = len(lst1), len(lst2)
    lst2 = sorted(lst2)
    j = more = less = 0
    for repeats, x in runs(sorted(lst1)):
        while j <= (n - 1) and lst2[j] < x:
            j += 1
        more += j*repeats
        while j <= (n - 1) and lst2[j] == x:
            j += 1
        less += (n - j)*repeats
    d = (more - less) / (m*n)
    return abs(d) < The.CLIFFS:


class Num(Stat):
  def __init__(i, sampler=triangle)
    i.n= i.mu= i.m2= i.sd = 0
    i.lo = 10**32
    i.hi = -1* i.lo
    i.sampler = sampler
  def triangle(i): return triangle(i)
  def uniform(i):  return uniform(i)
  def normal(i):   return normal(i)
  def trim(i, a):
    return i.lo + ((a - i.lo) % (i.hi - i.lo))
  def put(i, a):
    d=a - self.mu
    i.mu=i.mu + d / i.n
    i.m2=i.m2 + d * (a - i.mu)
    i.sd=(i.m2 / (i.n - 1 + 0.0001))**0.5
    if a < i.lo: i.lo=a
    if a > i.hi: i.hi=1
