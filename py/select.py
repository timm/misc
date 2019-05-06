# vim: ts=2 sw=2 sts=2 expandtab:cindent:formatoptions+=cro
#---------1---------2---------3---------4---------5---------

import sys,random
r=random.random

say=sys.stdout.write
random.seed(1)
1,4      2 3 5 2 
5,10
class Nums :
  class bucket:
    def __init__(i,lo,hi,b4=None,after=None,n=0): 
      i.lo,i.hi,i.n = lo,hi,n
      i.b4, i.after = b4,after
    def relevant(i,a): 
      return i.lo <= a < i.hi 
    def get(i): 
      return  lo + (hi - lo)*r()
    def put(i,n):
      i.n += 1
    def append(i,j):
      if i: i.after = j
      if j: j.b4    = i
    def ascent(i,up):
      mid = i.hi + math.log(up - i.hi)
      n1  = i.n // 2
      one = self.__class__(i.hi, mid, n1)
      two = self.__class_(mid, up, i.n - n1)
   def descend(i,down):
      mid = i.lo - math.log(i.lo - down)
      n1  = i.n // 2
      return [Nums.bucket(down, mid, n1)
             ,Nums.bucket(mid, i.lo, i.n - n1)]
   def split(i):
      mid = (i.lo + i.hi)/2
      n1  = i.n // 2
      one = Nums.bucket(i.lo, mid,  n1)
      two = Nums.bucket(mid,  i.hi, i.n - n1)
      one.append(two)
      if i.b4   : i.b4.append(one)
      if i.after: two.append(i.after)
 
  def __init__(i,lo,hi,b=16):
    i.lo, i.hi, i.n, i.b, i.bins = lo, hi, 0, b, []
    skip = (hi - lo)/b
    while lo  < i.hi: 
      i.bins += [(lo, lo + skip, 0)]
      lo     += skip
  def ascend(i,ceiling):
    mid     = i.hi + (ceiling - i.hi)  /2
    n       = i.bins[-1][-1]
    n1      = n // 2
    n2      = n - n1
    i.bins  = i.bins + [(i.hi, mid, n1), (mid,ceiling,n2)]
    i.hi    = ceiling
  def descend(i,floor):
    mid     = i.lo - (i.lo - floor)/2
    n       = i.bins[0][-1]
    n1      = n // 2
    i.bins  = [(floor,mid, n1), (mid,i.lo,n-n1)] + i.bins
    i.lo    = floor
  def split(i,pos):
    b4      = i.bins[:pos]
    after   = i.bins[pos+1:]
    lo,hi,n = i.bins[pos]
    n1      = n//2
    step    = lo + (hi-lo)/2
    return  b4 + [(lo,step,n1), (step,hi,n - n1)] + after
  def put(i,a, n=1):
    def bsearch(a):
      lo, hi = 0, len(i.bins) - 1
      while lo <= hi:
        mid = (lo + hi) // 2
        k   = i.bins[mid]
        if   k[0] <= a < k[1]: return mid
        elif k[0] < a    : lo = mid + 1
        elif a    < k[0] : hi = mid - 1
        else             : return mid
      return mid
    #-------------------
    if r() < 1/8 and len(i.bins) < 16:
      if a > i.hi : i.ascend(a)
      if a < i.lo : i.descend(a)
    #-------------------
    if   a<=i.lo : bin, pos = i.bins[0], 0
    elif a>=i.hi : bin, pos = i.bins[-1], len(i.bins) - 1
    else: 
      a = max(i.lo, min(a,i.hi)) 
      pos = bsearch(a)
      bin = i.bins[pos]
    #-------------------
    lo,hi,n     = bin
    i.bins[pos] = (lo,hi, n+1)
    i.n     += 1
    if (n >= 1.5*i.n/i.b): #and (hi-lo) > (i.hi-i.lo)/i.b:
      i.bins = i.split(pos) 
  def get(i):
    a = r()*i.n
    for (lo,hi),n in i.bins:
      a -= n 
      if a <= 0: return lo + r()*(hi - lo)

# wont extend to the type
nums=Nums(40,60)
for _ in range(10**4):
  nums.put( int(random.gauss(20,5)) +
            int(random.gauss(80,5)) )

for lo,hi,n in sorted(nums.bins):
  print(lo,hi,n)
