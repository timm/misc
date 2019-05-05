# vim: ts=2 sw=2 sts=2 expandtab:cindent:formatoptions+=cro
#---------1---------2---------3---------4---------5---------

import sys,random
r=random.random

say=sys.stdout.write
random.seed(1)

class Nums :
  def __init__(i,lo,hi,b=8):
    i.lo, i.hi, i.n, i.b, i.bins = lo, hi, 0, b, []
    skip = (hi - lo)/b
    while lo  < i.hi: 
      i.bins += [(lo, lo + skip, 0)]
      lo     += skip
  def ascend(i,ceiling):
    say("+")
    mid     = i.hi + (ceiling - i.hi)  /2
    n       = i.bins[-1][-1]
    n1      = n // 2
    n2      = n - n1
    i.bins  = i.bins + [(i.hi, mid, n1), (mid,ceiling,n2)]
    i.hi    = ceiling
  def descend(i,floor):
    say("-")
    mid     = i.lo - (i.lo - floor)/2
    n       = i.bins[0][-1]
    n1      = n // 2
    n2      = n - n1
    i.bins  = [(floor,mid, n1), (mid,i.lo,n2)] + i.bins
    i.lo    = floor
  def put(i,a, n=1):
    def bsearch(a):
      lo, hi = 0, len(i.bins) - 1
      while lo <= hi:
        mid = (lo + hi) // 2
        k   = i.bins[mid][0]
        if   k < a : lo = mid + 1
        elif a < k : hi = mid - 1
        else       : return mid
      return mid
    #-------------------
    if r() < 1/8:
      if a > i.hi : i.ascend(a)
      if a < i.lo : i.descend(a)
    #-------------------
    if   a<=i.lo : bin, pos = i.bins[0], 0
    elif a>=i.hi : bin, pos = i.bins[-1], len(i.bins) - 1
    else: 
      a = max(i.lo, min(a,i.hi)) 
      pos = bsearch(a)
      bin = i.bins[pos]
      print(a,pos,bin)
    #-------------------
    lo,hi,n     = bin
    i.bins[pos] = (lo,hi, n+1)
    i.n     += 1
    if (hi - lo) > (i.hi-i.lo)/32 and n >= 2*i.n/i.b:
      i.bins = i.split(pos) 
  def split(i,pos):
    say("/")
    b4      = i.bins[:pos]
    after   = i.bins[pos+1:]
    lo,hi,n = i.bins[pos]
    n1      = n//2
    n2      = n - n1
    step    = lo + (hi-lo)/2
    return  b4 + [(lo,step,n1), (step,hi,n2)] + after
  def get(i):
    a = r()*i.n
    for (lo,hi),n in i.bins:
      a -= n 
      if a <= 0: return lo + r()*(hi - lo)

nums=Nums(30,70,4)
for _ in range(30):
  nums.put( int(random.gauss(50,20)) )

print("")
for lo,hi,n in sorted(nums.bins):
  print(lo,hi,n)
