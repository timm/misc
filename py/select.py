from random import random as r

 
class bin:
    def put(i): i.n += 1
    def splitp(i): return False
    def split(i): return [i]

class symbin(bin):
    def __init__(i,x): i.n,i.x = 0,x,{}
    def __repr__(i): return "%s %s" %(i.n, i.n)
    def minep(i,x) : return x==i.x
    def get(i): return i.x


class numbin(bin):
    def __init__(i,lo,hi): i.n,i.lo,i.hi = 0,i.lo,i.hi
    def __repr__(i): return "[%s:%s) %s" %(i.lo, i.hi, i.n)
    def minep(i,x) : return i.lo <= x < i.hi
    def splitp(i,bins,n): return i.n > n and r() < 1/bins

    def get(i): return i.lo + r()*(i.hi - i.lo)

class symbols:
    def __init__(i): i.d={}; i.n=0
    def put(i,x,n=1): d[x] = d.get(x,0) + n
    def get(i):
      c=r()*i.n
      for k,v in d.items():
         c -= v 
         if c <= 0:
           return k

class numbers:
    def __init__(i,lo,hi,n): 
       a= lo
       i.step=(hi - lo)/n
       i.counts=symbols()
       while a < hi:
         synbols.put(a,0)
         a += step

       fori.d={}; i.n=0
    def put(i,x,n=1): d[x] = d.get(x,0) + n
    def get(i):
      c=r()*i.n
      for k,v in d.items():
         c -= v 
         if c <= 0:
           return k



def plus(x,d):
    d[x] = d.get(x,0) + 1

# if b bins and if b size > 1/n then at prob 1/b split that bucket
# if bucket cotnaisn one item, return it. if it contains two pull at random
def one(d):
   sum =0
   for _,v in d.items(): sum += v
   n=r()*sum
   for k,v in d.items():
     n -= v 
     if n <= 0:
       return k

d={}
for x in '0123456789': plus(x,d)
e={}
n=100
[plus(one(d),e) for _ in range(n)]
for k,v in sorted(e.items()):
    print(k,v/n)
