# vim: set ts=2 sw=2 sts=2 et :
import math, random, sys,re 
from types import SimpleNamespace as o

the = o(seed       = 1234567891,
        bootstraps = 512,
        confCliffs = 0.197, # cliffs' small,med,large: .11, .28, .43
        ConfBoost  = 0.95,
        Kohen     = 0.35)

#--------------------------------------------------------------------
def same():
  return lambda xs,ys: cliffs(xs,ys,the.confCliffs) and \
                       bootstrap(xs,ys,the.boostraps,the.ConfBoost)

#--------------------------------------------------------------------
def cliffs(xs,ys, conf=the.confCliffs):
  "Effect size. Tbl1 of doi.org/10.3102/10769986025002101"
  n,lt,gt = 0,0,0
  for x in xs:
    for y in ys:
      n += 1
      if x > y: gt += 1
      if x < y: lt += 1
  return abs(lt - gt)/n  < conf 

#--------------------------------------------------------------------
# Non-parametric significance test from 
# Chp20,doi.org/10.1201/9780429246593. Distributions are the same 
# if, often, we `_obs`erve differences just by chance. We center both 
# samples around the combined mean to simulate what data might look 
# like if xs and ys came from the same population.
def stat(t):
  m = sum(t) / len(t)
  sd = (sum((x - m)**2 for x in t) / (len(t) - 1))**.5
  return m, sd, len(t)

def obs(a, b):
  ma, sda, na = stat(a)
  mb, sdb, nb = stat(b)
  return abs(ma - mb) / ((sda**2 / na + sdb**2 / nb)**.5 + 1E-32)

def bootstrap(xs, ys, b=the.bootstraps, conf=the.ConfBoost):
  mx,_,nx = stat(xs)
  my,_,ny = stat(ys)
  mxy     = (mx * nx + my * ny) / (nx + ny)
  xhat    = [x - mx + mxy for x in xs]
  yhat    = [y - my + mxy for y in ys]
  ref     = obs(xs, ys)
  C       = lambda t: random.choices(t, k=len(t))
  more    = sum(obs(C(xhat), C(yhat)) > ref for _ in range(b))
  return more / b >= (1 - conf)

#--------------------------------------------------------------------
def ranks(d, eps=None, reverse=False):
  if not eps:
    _, sd, _ = stat([x for l in d.values() for x in l])
    eps = the.Kohen * sd
  def bag(k, mu, sd, n): return o(key=k, mu=mu, sd=sd, n=n, all=d[k])
  def same(x, y): return abs(x.mu-y.mu)<eps or \
                       cliffs(x.all,y.all) and bootstrap(x.all,y.all)
  l, out = [], {}
  for now in sorted([bag(k, *stat(l)) for k, l in d.items()],
                    key=lambda z: z.mu, reverse=reverse):
    if l and same(l[-1], now):
      l[-1] = bag(l[-1].key, *stat(l[-1].all + now.all))
    else:
      l += [now]
    now.rank = chr(96 + len(l))
    out[new.key] = now
  return out

#--------------------------------------------------------------------
class Confusion:
  def __init__(i, lbl="-", n=0): 
   i.label, i.tn = lbl, n; i.tp = i.fp = i.fn = 0

  def add(i, want, got, x):
    if x == want: i.tp += (got == want); i.fp += (got != want)
    else:         i.fn += (got == x);    i.tn += (got != x)

  def finalize(i):
    p = lambda y,z: int(100 * y / (z or 1e-32))
    i.pd   = p(i.tp, i.tp + i.fp)
    i.pf   = p(i.fn, i.fn + i.tn)
    i.prec = p(i.tp, i.tp + i.fn)
    i.acc  = p(i.tp + i.tn, i.tp + i.fp + i.fn + i.tn)
    return i

class Confusions:
  def __init__(i): i.data, i.n = {}, 0

  def add(i, want, got):
    for lbl in (want, got): 
      i.data[lbl] = i.data.get(lbl) or Confusion(lbl, i.n)
    for s in i.data.values(): s.add(want, got, s.label)
    i.n += 1; return i
  
  def finalize(i): [s.finalize() for s in i.data.values()]; return i

  def summary(i):
    out = Confusion()
    for s in i.data.values():
      w       = (s.tp + s.fp) / i.n
      out.tp += s.tp * w
      out.fp += s.fp * w
      out.fn += s.fn * w
      out.tn += s.tn * w
    return out.finalize()

#--------------------------------------------------------------------
def two(n,delta):
  one = [random.gauss(10,1) for _ in range(n)]
  two = [x + delta for x in one]
  return one, two

def eg__the(): print(the)

def eg__stats():
   def c(b): return 1 if b else 0
   G  = random.gauss
   R  = random.random
   n  = 50
   b4 = [G(10,1) for _ in range(n)]
   d  = 0
   while d < 2:
     now = [x+d*R() for x in b4]
     b1  = cliffs(b4,now)
     b2  = bootstrap(b4,now)
     print(dict(d=f"{d:.3f}", cliffs=c(b1), boot=c(b2),  agree=c(b1==b2)))
     d  += 0.1

def eg__rank():
   G  = random.gauss
   n=100
   d=dict(asIs  = [G(10,1) for _ in range(n)],
          copy1 = [G(20,1) for _ in range(n)],
          now1  = [G(20,1) for _ in range(n)],
          copy2 = [G(40,1) for _ in range(n)],
          now2  = [G(40,1) for _ in range(n)])
   for x in ranks(d):
      print(o(key=x.key, rank=x.rank, num=x.mu))

if __name__ == "__main__":
  for n,arg in enumerate(sys.argv):
    if (fn := globals().get(f"eg{arg.replace('-', '_')}")):
      random.seed(the.seed)
      fn()
