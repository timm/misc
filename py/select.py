from random import random as r

def plus(x,d):
    d[x] = d.get(x,0) + 1

def one(d):
   sum =0
   for _,v in d.items(): sum += v
   n=r()*sum
   for k,v in d.items():
     n -= v 
     if n <= 0:
       return k

d={}
for x in 'abcabaee': plus(x,d)
e={}
n=10000
[plus(one(d),e) for _ in range(n)]
for k,v in sorted(e.items()):
    print(k,v/n)
