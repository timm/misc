from random import random as r

# split full bins
# if less than first, split downwards by 2 bins. one of size w, one down to newlow
# if more than last, split updates by 2 bins. one of size w, one up to newhigh
# for nums, bin chop
# for syms, exact. 
# two finds of bins
# need to call this tabu
class bins :
    def __init__(i,lo,hi,b=16): 
      i.n, i.d, i.lo, i.hi, i.w = 0, {}, lo, hi, (i.hi - i.lo)/b
    def bin(i,a):
      return i.hi - i.w if a == i.hi else i.lo + int((a - i.lo) / i.w)
    def nump(i,a):
      return isinstance(a, (int, float))
    def put(i,a):
      i.n += 1
      if i.nump(a): a = i.bin(a)
      i.d[a] = i.d.get(a,0) + 1
    def get(i):
      a = r()*i.n
      for k,v in i.d.items():
        a -= v 
        if a <= 0:
          return k+r()*i.w if i.nump(k) else k

for x in '0123456789': plus(x,d)
e={}
n=100
[plus(one(d),e) for _ in range(n)]
for k,v in sorted(e.items()):
    print(k,v/n)


