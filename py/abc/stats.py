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
def ranks(d, eps=None):
  if not eps:
    _, sd, _ = stat([x for l in d.values() for x in l])
    eps = the.Kohen * sd
  bag = lambda k, mu, sd, n: o(key=k, mu=mu, sd=sd, n=n, all=d[k])
  same = lambda x, y: abs(x.mu - y.mu) < eps or \
                      cliffs(x.all, y.all) and bootstrap(x.all, y.all)
  tmp, out = [], []
  for now in sorted([bag(k, *stat(l)) for k, l in d.items()],
                    key=lambda z: z.mu):
    if tmp and same(tmp[-1], now):
      tmp[-1] = bag(tmp[-1].key, *stat(tmp[-1].all + now.all))
    else:
      tmp += [now]
    now.rank = chr(96 + len(tmp))
    out += [now]
  return out


#--------------------------------------------------------------------
class Abcd:
  def __init__(i,kl="-",a=0):
    i.a,i.txt = a,kl
    i.b = i.c = i.d = 0
  def add(i, want, got, x):
    if x == want:   i.d += (got == want); i.b += (got != want)
    else:           i.c += (got == x);    i.a += (got != x)
  def ready(i):
    p      = lambda y, z: int(100 * y / (z or 1e-32))
    i.pd   = p(i.d,       i.d + i.b)
    i.pf   = p(i.c,       i.c + i.a)
    i.prec = p(i.d,       i.d + i.c)
    i.acc  = p(i.d + i.a, i.a + i.b + i.c + i.d)
    return i

def abcdReady(state):
  for abcd in state.stats.values(): abcd.ready()
  return state

def abcdWeighted(state):
  out = Abcd()
  for abcd in state.stats.values():
    w = (abcd.b + abcd.d)/state.n
    out.a += abcd.a*w
    out.b += abcd.b*w
    out.c += abcd.c*w
    out.d += abcd.d*w
  return out.ready()

def abcds(want, got, state=None):
  "usage: state=abcds(want,got,state)"
  state = state or o(stats={},  n=0)
  for L in (want, got):
    state.stats[L] = state.stats.get(L) or Abcd(L,state.n)
  for x, s in state.stats.items():
    s.add(want, got, x)
  state.n += 1
  return state

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
