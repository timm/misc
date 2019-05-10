import random
import time

r= random.random


class fred(object):
   def __init__(self): self.lst = [r() for _ in range(20)]
   #def __hash__(self): return self.id
   #def __eq__(self, x): print(1); return x is self
   #def __ne__(self, x): print(1); return not self == x #x is not self

n=10**6
some = [fred() for _ in range(n)]
print(1)
start_time = time.time()
s=set()
[s.add(x) for x in some]
n1=len(s)
[s.remove(x) for x in some]
print("--- %s seconds ---" % ((time.time() - start_time)/n))
print(n1,len(s))
