import random
import time

r= random.random

class fred(object):
   def __init__(self): self.lst = [r() for _ in range(20)]

n          = int(0.5*10**6)
some       = [fred() for _ in range(n)]
start_time = time.time()
s          = set()

[s.add(x) for x in some]

n1         = len(s)

[s.remove(x) for x in some]

t          = time.time() - start_time

print("--- %s seconds %s rate---" % (t, t/n))
print(n1,len(s))
