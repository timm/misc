import random
r = random.random

def p(s=2,hi=0.25,d=10,repeats=10000):
  tried,passed = 0.0001,0
  for _ in range(repeats):
    m = int(r()*20) + 1
    x = r()*hi
    y = r()*hi
    a = x**m
    b = y**m
    if b>0 and a>0 and x>=a and y>=b and a<1 and b<1 and x<1 and y<1:
      if  x > y*d and m > 1:
        tried = tried+ 1
        if (a/x)**s/(a/x + b/y) < a**s/(a+b):
          print(s,m,d,x,x/y)

def s(z):  return int(z*100)

hi=1
while hi>0.1:
   for d in [1,2,4,8,16,32]:
     hi *= 0.8
     #x = p(s=1,d=d,hi=hi)
     y = p(s=1.2,d=d,hi=hi)
     #print(s(hi), d,s(x), s(y))
