from random import random as r

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
