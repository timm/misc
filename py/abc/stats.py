def same(cliff=None, n=None, conf=None):
  n     = n or the.bootstrap
  conf  = conf or the.Boots
  cliff = cliff or the.Cliifs
  return lambda xs,ys:cliffs(xs,ys,cliff) and bootstrap(xs,ys,n,conf)

#--------------------------------------------------------------------
def cliffs(xs,ys, cliff):
  "Effect size. Tb1 of doi.org/10.3102/10769986025002101"
  n,lt,gt = 0,0,0
  for x in xs:
    for y in ys:
      n += 1
      if x > y: gt += 1
      if x < y: lt += 1
  return abs(lt - gt)/n  < cliff # 0.197)  #med=.28, small=.11

#--------------------------------------------------------------------
# Non-parametric significance test from 
# Chp20,doi.org/10.1201/9780429246593. Distributions are the same 
# if, often, we `_obs`erve differences just by chance. We center both 
# samples around the combined mean to simulate what data might look 
# like if xs and ys came from the same population.
def stat(t):
  m = sum(t) / len(t)
  s = (sum((x - m)**2 for x in t) / (len(t) - 1))**.5
  return m, s, len(t)

def obs(a, b):
  ma, sa, na = stat(a)
  mb, sb, nb = stat(b)
  return abs(ma - mb) / ((sa**2 / na + sb**2 / nb)**.5 + 1E-32)

def bootstrap(xs, ys, b=1000, conf=0.05):
  sx, sy = sum(xs), sum(ys)
  nx, ny = len(xs), len(ys)
  mxy    = (sx + sy) / (nx + ny)
  xhat   = [x - sx / nx + mxy for x in xs]
  yhat   = [y - sy / ny + mxy for y in ys]
  ref    = obs(xs, ys)
  C      = lambda t,k: random.choices(t, k=k)
  more   = sum(obs(C(xhat, nx), C(yhat, ny)) > ref for _ in range(b))
  return more / b >= (1 - conf)

#--------------------------------------------------------------------
def sk(rxs, same, eps=0, reverse=False):
  "Dict[key,List[float]] -> List[(rank,key,vals,n,mu)]"
  def _cut(cuts, cut=None):
    N = sum(n for *_,n,_ in cuts)
    M = sum(mu*n for *_,n,mu in cuts) / N
    best = s1 = n1 = 0
    for j, (*_,n,mu) in enumerate(cuts[:-1]):
      n1, s1 = n1 + n, s1 + mu * n
      m1 = s1 / n1
      m2 = (M * N - s1) / (N - n1)
      gain = n1/N * (m1 - M)**2 + (1 - n1/N) * (m2 - M)**2
      if abs(m1 - m2) > eps and gain > best:
        best, cut = gain, j+1
    return cut

  def _div(cuts, rank=0):
    if (cut := _cut(cuts)) is not None:
      L, R = cuts[:cut], cuts[cut:]
      if not same([x for _,_,v,_,_ in L for x in v],
                  [x for _,_,v,_,_ in R for x in v]):
        return _div(R, _div(L, rank)+1)
    cuts[:] = [(rank, k, v, n, m) for _,k,v,n,m in cuts]
    return rank

  cuts = [(0,k,v,len(v),sum(v)/len(v)) for k,v in rxs.items()]
  cuts.sort(key=lambda r: r[4], reverse=reverse)
  _div(cuts)
  return sorted(cuts)
