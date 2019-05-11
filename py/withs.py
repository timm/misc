import random 
r=random.random
#random.seed(1)  <== important! somewhere, you must maintain seed control

# add a support set. and an untrain
# add a simulated annealler
# add a rule learner (greedy) 
# remember to add things at strength 0 if we like them (otherwise
#      we'll never see that thing)
# add ffts and doing within/without at each level
class bins:
  """Train a distributions with samples. Draw from 'within'
  that to recreate it. Draw from 'without' to avoid it."""
  def __init__(i)    : i.n, i.d = 0, {}
  def train(i,z, n=1): i.n += n; i.d[z] = i.d.get(z,0) + n
  def within(i)      : return i.get(lambda z: z/i.n)
  def without(i)     : return i.get(lambda z: (i.n - z)/i.n)
  def get(f):
    tmp = r()
    for k,v in i.d.items():
      tmp -= f(v)
      if tmp <= 0: return k

class nums:
  "Using 'bins', keep track of nums (rounded to a width i.w)"
  def __init__(i,w=0.12): i.w, i.bins = w, bins
  def within(i)       : return i.bins.within()   + r()*i.w
  def without(i)      : return i.bins.without()  + r()*i.w
  def train(i, z, n=1): return i.bins.train(z//i.w * i.w, n)

class around:
  """Using 'nums', whenever you train on z, spread 
  some of that effect around the nearby region."""
  def __init__(i, w      = 0.12,
                  nearby = [-1.5, -0.5, 0, 0.5, 1.5],
                  fxs    = [1,     2,   6, 2,   1]):
    i.nums = nums(w)
    i.todo = [(n, fx/sum(fxs)) for n,fx in zip(nearby,fxs)]
  def within(i)       : return i.nums.within()
  def without(i)      : return i.nums.without()
  def train(i, z, n=1):
    for nearby, effect in i.todo:
      i.nums.train( z + i.nums.w*nearby, n*effect )

class norm(object):
  def __init__(i, lo=10**32, hi=-10**32): i.lo,i.hi = lo,hi
  def __call__(i,z):
    i.lo, i.hi = min(z,i.lo), max(z,i.hi)
    return (z - i.lo) / (i.hi - i.lo + 10**-32)

norm1=norm()
for _ in range(100):
  print(norm1(10*r()))
