#!/usr/bin/env python3
# vim : nospell filetype=py ts=2 sw=2 sts=2  et  :



class Thing(Mine):
  def xpectv(i,j):
    n = i.n + j.n
    return i.n/n*i.variety() + j.n/n*j.variety()

  def __add__(i,x):
    if x == THE.skip: return x
    i.n += 1
    i.add(x)
    return x

class Num(Thing):
  "Track numbers seen in a column"
  def __init__(i, inits=[], pos=0,txt="",w=1):
    i.pos, i.txt = pos, txt
    i.n, i.w = 0, w
    i.mu, i.m2 = 0, 0
    i.lo, i.hi = 10 ** 32, -10 ** 32
    [i + x for x in inits]
  def variety(i): return i.sd()
  def expect(i): return i.mu
  def sd(i):
    return 0 if i.n < 2 else (i.m2 / (i.n - 1 + 10 ** -32)) ** 0.5
  def add(i, x):
      if x < i.lo:
        i.lo = x
      if x > i.hi:
        i.hi = x
      d = x - i.mu
      i.mu += d / i.n
      i.m2 += d * (x - i.mu)

  def __sub__(i, x):
      if i.n < 2:
        i.n, i.mu, i.m2 = 0, 0, 0
      else:
        i.n -= 1
        d = x - i.mu
        i.mu -= d / i.n
        i.m2 -= d * (x - i.mu)

class Sym(Thing):
  def __init__(i,inits=[],pos=0,txt="",w=1):
   i.pos, i.txt = pos, txt
   i.n, i.w = 0, w
   i.mode=None
   i.most=0
   i.cnt = {}
   [i + x for x in inits]
  def add(i,x):
   new = i.cnt.get(x,0) + 1
   i.cnt[x] = new
   if new > i.most:
     i.mode, i.most = x,new
  def ent(i):
    e=0
    for v in i.cnt.values():
      p = v/i.n
      e += -1*p*math.log(p,2)
    return e


