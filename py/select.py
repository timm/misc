from random import random as r

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
