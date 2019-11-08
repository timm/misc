import random
r = random.random
m,n=1,1

def p(s=2,hi=1,repeats=100000):
  global m,n
  for _ in range(repeats):
    m = m+1
    x = r()*hi
    y = r()*hi
    a = r()*hi
    b = r()*hi
    #if b>0 and a>0 and x>=a and y>=b and a<1 and b<1 and x<1 and y<1:
    if a>b  and x>=a and y>=b  and x>y:  # and x > y:
        if (a/x)**s/(a/x + b/y) > a**s/(a+b):
          n=n+1
          yield dict(r=int(x/a),x=x,xy=round(x/y,1))

def ss(z):  return int(z*100)

random.seed(0)
hi=.5
while hi>0.05:
  hi *= 0.8
  s=2/0.8
  print("")
  while s >=1:
   s*=0.8
   m,n=1,1
   for x in p(s=s,hi=hi): True #print(x)
   print(hi,s,n/m,m)
