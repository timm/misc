import random
r=random.random
random.seed(1)
any=random.choice

class bins:
  def __init__(i)   : i.n, i.d = 0, {} 
  def put(i, z, n=1): i.n += n; i.d[z] = i.d.get(z,0) + n
  def want(i)       : return i.get(lambda z: z)
  def dont(i)       : return i.get(lambda z: i.n - z)
  def get(f):
    tmp = r()
    for k,v in i.d.items():
      tmp -= f(v)/i.n
      if tmp <= 0: return k

class nums:
  def __init__(i, w=0.12): i.w, i.bins = w,bins
  def want(i)            : return i.bins.want() + r()*i.w
  def dont(i)            : return i.bins.dont() + r()*i.w
  def put(i, z, n=1)     : i.bins.put( z//i.w * i.w, n)

class near:
  def __init__(i, w=0.12,
                  spreads = [-1.5, -0.5, 0, 0.5, 1.5],
                  weights = [1,     2,   6, 2,   1]):
    i.nums = nums(w)
    i.todo = [(s,w/sum(weights)) for 
              s,w in zip(i.spreads,i.weights)]
  def want(i) : return i.nums.want()
  def dont(i) : return i.nums.dont()
  def put(i, z, n=1):
    for spread,weight in i.todo:
      i.nums.put( z + i.nums.w*spread, n*weight )
