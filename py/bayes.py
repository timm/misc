import random
r = random.random

def p(s=2,hi=0.25,d=10,repeats=10000):
  tried,passed = 0.0001,0
  for _ in range(repeats):
    m = int(r()*5) + 1
    n = int(r()*5) + 1
    x = r()*hi
    y = r()*hi/d
    a = r()*hi
    b = r()*hi
    if b> 0 and a> 0 and x>=a and y>=b and a<1 and b<1 and x<1 and y<1:
      tried = tried+ 1
      if (a/x)**s/(a/x + b/y) > a**s/(a+b):
        passed=passed+1
  return passed/tried

def s(z):  return int(z*100)

hi=1
while hi>0.1:
   for d in [1,2,4,8,16]:
     hi *= 0.8
     x = p(s=1,d=d,hi=hi)
     y = p(s=2,d=d,hi=hi)
     if y> x:
       print(s(hi), d,s(x), s(y))
