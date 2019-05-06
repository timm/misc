# vim: ts=2 sw=2 sts=2 expandtab:cindent:formatoptions+=cro
#---------1---------2---------3---------4---------5---------

import sys,random
r=random.random

say=sys.stdout.write
random.seed(1)
1,4      2 3 5 2 
5,10
# do sorted true if the first place
class Nums :
  class bucket:
    def __init__(i,lo,hi,n=0): i.lo,i.hi,i.n = lo,hi,n
    def get(i)               : return  lo + (hi - lo)*r()
    def put(i,n=1):          : i.n += n
    def split(i):
      mid = (i.lo + i.hi)/2
      n1  = i.n // 2
      return [Nums.bucket(i.lo, mid,  n1)
             ,Nums.bucket(mid,  i.hi, i.n - n1)]
        
 
  def __init__(i,lo=0,hi=1,b=16):
    i.lo, i.hi, i.n, i.bins = lo, hi,  0, []
    inc = (i.hi - i.lo)/b
    while lo  < hi: 
      i.bins += [Num.buckets(lo, lo + inc)]
      lo     += inc
  def ascend(i,up):
      a = i.lo + math.log(up - i.lo)
      return [Nums.bucket(i.lo,a), Nums.bucket(mid,up,1)]
   def descend(i,down):
      a = i.hi - math.log(i.hi - down)
      return [Nums.bucket(down,a,1),Nums.bucket(a,i.hi)]
   def sort(i,lst):
     i.bins = sorted(lst, key=lambda b:b.n, reverse=True)
   def put(i,a, n=1):
     if a < i.lo: i.lo = a; i.sort(i.descend(a)+i.bins)
     if a > i.hi: i.hi = a; i.sort(i.bins + i.ascend(a))
     for c,b in enumerate(i.bins):
       if b.lo <= a < b.hi: break
     i.n += na XXXXX
     i.bins[c].put(n)
     if i.bins[c].n > 1.5*i.n/len(i.bins):
        i.bins = i.bins[:c-1] + i.bins[c].split() + i.bins[c:]
       

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
