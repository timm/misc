import random,math
from collections import Counter

def r1(x): return round(x,1)


def pdf(m=0,s=1):
  while True:
    x1 = 2.0* random.random() -1
    x2 = 2.0* random.random() -1
    w = x1*x1 + x2*x2
    if w<1: break
  x=x1   
  w=math.sqrt((-2*math.log(w))/w)
  y1 = x*w
  return m + y1*s

def norm(m=0,s=1,r=1000,p=1):
  lst = sorted([pdf(m,s) for _ in range(r)])
  lo,hi = lst[0],lst[-1]
  return [round((x -lo)/(hi-lo),p) for x in lst]


d={}
for x in range(10):
  for y in range(10):
    for z in range(10):
      d[(x,y,z)] = 0


def nums(m=0,s=1,r=100):
  for x in norm(m=m,s=s,r=r):
    for y in norm(m=m,s=s,r=r):
      for z in norm(m=m,s=s,r=r):
        k=(x,y,z)
        d[k] = d.get(k,0) + 1
  return [v/(r**3) for k,v in sorted(d.items(), reverse=True,key=lambda kv: kv[1])]

def nums1(m=0,s=1,r=100):
  for x in norm(m=m,s=s,r=r,p=3):
    for y in norm(m=m,s=s,r=r,p=3):
      for z in norm(m=m,s=s,r=r,p=3):
        k=(x,y,z)
        d[k] = d.get(k,0) + 1
  return d
  
for e in [.15,.1,.05]:
  av=0
  for (x,y,z),n in nums1(s=1,r=100).items(): 
    if  abs(x-.5)<e  and abs(y-.5)<e and abs(z-.5)<e :av+=1
  print(e,f"{av/100**3*100:5.2f}" )

import matplotlib.pyplot as plt


# plt.plot(nums(s=1,r=100),label="1")
# plt.plot(nums(s=.1,r=100),label=".1")
# plt.plot(nums(s=6,r=100),label="6");  
# plt.legend(loc="upper right")
# plt.show()
#

