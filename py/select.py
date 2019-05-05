# vim: ts=2 sw=2 sts=2 expandtab:cindent:formatoptions+=cro
#---------1---------2---------3---------4---------5---------

from random import random as r

def bsearch(lst, a, key = lambda b: b):
  lo, hi = 0, len(lst) - 1
  while lo <= hi:
    mid = (lo + hi) // 2
    b   = lst[mid]
    c   = key(b)
    if   c < a : lo = mid + 1
    elif a < c : hi = mid - 1 
    else       : return c,mid
  assert False 'key no found'

class nums :
    def __init__(i,lo,hi,b=8):
      i.lo, i.hi, i.n, i.b, i.bins = lo, hi, 0, b, []
      skip = (hi - lo)/b
      while lo  < i.hi: 
        i.bins += [(lo, lo + skip, 0)]
        lo     += skip
    def ascend(i,ceiling):
      mid     = (i.hi + ceiling)  /2
      n       = i.bins[-1]
      i.bins += [(i.hi, mid, n/2), (mid,ceiling,n/2)]
      i.hi    = ceiling
    def descent(i,floor):
      mid     = (i.lo - floor)/2
      n       = i.bins[0]
      i.bins  = [(floor,mid, n/2), (mid,i.lo,n/2)] + i.bins
      i.lo    = floor
    def put(i,a, n=1):
      if r() < 1/i.b:
        if a > i.hi : i.ascend(a)
        if a < i.lo : i.descend(a)
      bin,pos = bsearch(i.bins, max(i.lo, min(a,i.hi)), 
                        key = lambda b:b[0])
      bin[-1] += 1
      i.n     += 1
      if bin[-1] > i.n/i.b:
        i.bins = i,split(pos) 
    def split(i,pos):
      b4      = i.bins[:pos]
      after   = i.bins[pos+1:]
      lo,hi,n = i.bins[pos]
      half    = n/2
      step    = (lo+hi)/2
      return  b4 + [(lo,step,half), (step,hi,half)] + after
    def get(i):
      a = r()*i.n
      for (lo,hi),n in i.bins:
        a -= n 
        if a <= 0: return lo + r()*(hi - lo)

for x in '0123456789': plus(x,d)
e={}
n=100
[plus(one(d),e) for _ in range(n)]
for k,v in sorted(e.items()):
    print(k,v/n)


