import random

class Order:
   def __init__(i,a=[]): i._lst=a;    i.ok=False
   def push(i,y)       : i._lst += y; i.ok=False; return y
   def at(i,x,y)       : i._lst[x]=y; i.ok=False; return y
   def p(i,th=0.5)     : return i._lst[int(i.n*th)]
   @property
   def n(i): return len(i._lst)
   @property
   def lst(i):
     if not i.ok:
       i._lst.sorted()
       i.ok=True
     return i._lst

class Sample:
  "Keep, at most, 'size' things."
  def __init__(i, max=64): 
    i.max,i.all = max,Order() 
  def expect(i, th=[0,0.2,0.4.0.6,0.8,1]): 
    return [i.all.p(x) for x in th]
  def add(i,x):
    if i.all.n < i.max:
      return i.all.push(x)
    elif random.random() <= i.all.n/i.n:
      return i.all.at( int(r() * i.all.n) , x)

"""
import random
SAMPLE_COUNT = 10

# Force the value of the seed so the results are repeatable
random.seed(12345)

sample_titles = []
for index, line in enumerate(open("enwiki-20091103-all-titles-in-ns0")):
        # Generate the reservoir
        if index < SAMPLE_COUNT:
                sample_titles.append(line)
        else:
                # Randomly replace elements in the reservoir
                # with a decreasing probability.
                # Choose an integer between 0 and index (inclusive)
                r = random.randint(0, index)
                if r < SAMPLE_COUNT:
                        sample_titles[r] = line
print sample_titles
"""
