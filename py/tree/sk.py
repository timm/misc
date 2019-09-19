#!/usr/bin/env python3
# vim : nospell filetype=py ts=2 sw=2 sts=2  et  :

### new
#from thing import Sym,Num
#from the import THE
#from lib import *
from copy import deepcopy as kopy

#-------------------------------------------------------
def cliffsDeltaSlow(lst1,lst2,
                dull = [0.147, # small
                        0.33,  # medium
                        0.474 # large
                        ][0] ): 
  """Returns true if there are more than 'dull' difference.
     Warning: O(N)^2."""
  n= gt = lt = 0.0
  for x in lst1:
    for y in lst2:
      n += 1
      if x > y:  gt += 1
      if x < y:  lt += 1
  return abs(lt - gt)/n <= dull

def cliffsDelta(lst1, lst2, goal=0, 
                dull = [0.147, # small
                        0.33,  # medium
                        0.474 # large
                        ]):
  "Returns true if there are more than 'dull' differences"
  def runs(lst):
    for j,two in enumerate(lst):
      if j == 0: one,i = two,0
      if one!=two:
        yield j - i,one
        i = j
      one=two
    yield j - i + 1,two  
  #---------------------
  m, n = len(lst1), len(lst2)
  lst2 = sorted(lst2)
  j = more = less = 0
  for repeats,x in runs(sorted(lst1)):
    while j <= (n - 1) and lst2[j] <  x: j += 1
    more += j*repeats
    while j <= (n - 1) and lst2[j] == x: j += 1
    less += (n - j)*repeats
  d= (more - less) / (m*n) 
  return abs(d)  <= dull[goal]

#-------------------------------------------------------
def same(x): return x

class Mine:
  oid = 0

  def identify(i):
    Mine.oid += 1
    i.oid = Mine.oid
    return i.oid

  def __repr__(i):
    pairs = sorted([(k, v) for k, v in i.__dict__.items()
                    if k[0] != "_"])
    pre = i.__class__.__name__ + '{'
    def q(z):
     if isinstance(z,str): return "'%s'" % z 
     if callable(z): return "fun(%s)" % z.__name__
     return str(z)
    return pre + ", ".join(['%s=%s' % (k, q(v))])

#-------------------------------------------------------
class Rx(Mine):
  def fromDict(d):
    return [Rx(k,v) for k,v in d.items()]
  def sum(rxs):
    all = []
    for rx in rxs: 
      for val in rx.vals:
        all += [val]
    return Rx(vals=all)
  def show(rxs):
    tmp=Rx.sum(rxs)
    lo,hi=tmp.vals[0], tmp.vals[-1]
    for rx in sorted(rxs):
      print('%4s %10s %s' % (rx.rank, rx.rx, rx.tiles()))
  #----------------------------------------------
  def __init__(i, rx="",vals=[], key=same): 
    i.rx, i.vals = rx, sorted([x for x in vals if x != "?"])
    i.n = len(i.vals)
    i.med= i.vals[int(i.n/2)]
    i.mu= sum(i.vals)/i.n
    i.rank=1
  def __lt__(i,j):
    return i.med < j.med
  def __eq__(i,j):
    return cliffsDelta(i.vals,j.vals)
  def xpect(i,j,b4):
    n = i.n + j.n
    return i.n/n * (b4.med- i.med)**2 + j.n/n * (j.med-b4.med)**2
  def tiles(i,lo=0,hi=1):
     return  xtile(i.vals,lo,hi) 
  def __repr__(i):
    return '%4s %10s %s' % (i.rank, i.rx, i.tiles())

def skDemo(n=5) :
  return Rx.fromDict(
             dict(x1 =[ 0.34, 0.49 ,0.51, 0.6]*n,
                  x2  =[0.6  ,0.7 , 0.8 , 0.89]*n,
                  x3  =[0.13 ,0.23, 0.38 , 0.38]*n, 
                  x4  =[0.6  ,0.7,  0.8 , 0.9]*n,
                  x5  =[0.1  ,0.2,  0.3 , 0.4]*n))
#-------------------------------------------------------
  
def sk(rxs):
  sk1( sorted(rxs),Rx.sum(rxs),1)
  return rxs

def sk1(rxs,b4,rank):
  cut = left=right=None
  best = 0
  for j,rx in enumerate(rxs):
    if j > 0:
      left0  = Rx.sum( rxs[:j] )
      right0 = Rx.sum( rxs[j:] )
      now = left0.xpect(right0, b4)
      if now > best:
        if left0 != right0:
         best, cut,left,right = now,j,kopy(left0),kopy(right0)
  if cut:
    rank = sk1(rxs[:cut],left, rank)
    rank = sk1(rxs[cut:],right,rank+1)
  else:
    for rx in rxs:
      rx.rank = rank
  return rank

#-------------------------------------------------------
def pairs(lst):
    "Return all pairs of items i,i+1 from a list."
    last=lst[0]
    for i in lst[1:]:
         yield last,i
         last = i

def xtile(lst,lo,hi,width=50,
             chops=[0.1 ,0.3,0.5,0.7,0.9],
             #marks=["-" ," "," ","-"," "],
             marks=[" " ,"-","-","-"," "],
             bar="|",star="*",show=" %5.3f"):
  """The function _xtile_ takes a list of (possibly)
  unsorted numbers and presents them as a horizontal
  xtile chart (in ascii format). The default is a 
  contracted _quintile_ that shows the 
  10,30,50,70,90 breaks in the data (but this can be 
  changed- see the optional flags of the function).
  """
  def pos(p)   : return ordered[int(len(lst)*p)]
  def place(x) : 
    return int(width*float((x - lo))/(hi - lo+0.00001))
  def pretty(lst) : 
    return ', '.join([show % x for x in lst])
  ordered = sorted(lst)
  lo      = min(lo,ordered[0])
  hi      = max(hi,ordered[-1])
  what    = [pos(p)   for p in chops]
  where   = [place(n) for n in  what]
  out     = [" "] * width
  for one,two in pairs(where):
    for i in range(one,two): 
      out[i] = marks[0]
    marks = marks[1:]
  out[int(width/2)]    = bar
  out[place(pos(0.5))] = star 
  return '('+''.join(out) +  ")," +  pretty(what)

def _cliffsDelta():
  "demo function"
  lst1=[1,2,3,4,5,6,7]*100
  n=1
  for _ in range(10):
      lst2=[x*n for x in lst1]
      print(cliffsDelta(lst1,lst2),n) # should return False
      n*=1.03



if __name__ == "__main__":
  _cliffsDelta()
  n=5
  for _ in range(7):
    print()
    print(n)
    Rx.show(sk(skDemo(n)))
    n*=5



