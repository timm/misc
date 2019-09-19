#!/usr/bin/env python3
# vim : nospell filetype=py ts=2 sw=2 sts=2  et  :


from thing import Sym,Num
from the import THE
from lib import *
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
class Rx(Mine):
  @classmethod
  def fromDict(d):
    return [Rx(k,v) for k,v in d.items()]
  @classmethod
  def sum(rxs):
    all = []
    for rx in rxs: all += rx.vals
    return Rx(vals=all)
  #----------------------------------------------
  def __init__(i):(i,rx="",vals=[],key=same): 
    i.rx, i.vals = rx, [x for x in vals if x != THE.skip]
    i.n = len(i.vals)
    i.mu= sum(i.vals)/i.n
    i.rank=1
  def __lt__(i,j):
    return i,.mu < j.mu
  def __eq__(i,j):
    return cliffsDelta(i.mu,j.mu)

def skDemo() {
  return Rx.fromDict(
             dict(x1 =[ 0.34, 0.49 ,0.51, 0.6],
                  x2  =[0.6  ,0.7 , 0.8 , 0.9],
                  x3  =[0.15 ,0.25, 0.4 , 0.35], 
                  x4  =[0.6  ,0.7,  0.8 , 0.9],
                  x5  =[0.1  ,0.2,  0.3 , 0.4]))
#-------------------------------------------------------
  
def sk(rxs):
  lst    = sorted(lst,key=x)                      # sort on x
  above  = Rx.sum(all)
  sk1( sorted(rxs),here=1, there=len(rxs),above=Rx.sum(all))
  return rxs

def sk1(rxs,
  cut    = None
  print(yr)
  for n,one in enumerate(lst):
    xl + one;  yl + one                     # add to left
    xr - one;  yr - one                     # remove from right
    #-------------------------------------------------------------------------
    if xr.n > step:                         # if there is enough on right
        if xl.n >= step:                    # if there is enough of lect
          after = lst[n+1]
          if x( after ) != x( one ):        # if we can break here (since the x values change)
            if x(one) - xlo > tiny:         # if we are more than trivially more than xlo
              if xhi - x(one)  > tiny:      # if we are more than trivially away from xhi
                tmp = yl.xpect(yr)          # what would be the effect of cutting here?
                print(tmp,best)
                if tmp < best:                  # if the effect is good...
                   cut,best = x(lst[n+1]),tmp   # then cutting [0:cut] is the right cut
  return cut,best


def numdiv(lst,
        x=first,     # accessor for the indep variable
        y= last):    # accessor for the dep   variable
  lst    = sorted(lst,key=x)                      # sort on x
  ako    = Num if isNum( y(first(lst)) ) else Sym # how to create the right kind of counter for y
  xl, xr = Num(key=x), Num(lst,key=x)       # create counters for all x seen on left and right
  yl, yr = ako(key=y), ako(lst,key=y)       # create counters for all y seen in left and right
  tiny   = xr.sd() * THE.div.cohen          # ignore x-divisions less than "tiny"
  step   = int(len(lst)** THE.div.min )     # ensure that the divisions are at least of size "step"
  best   = yr.variety()                     # our initial survey of y. we want to do better than this
  xlo    = x( first(lst) )                  # least x value
  xhi    = x( last(lst) )                   # most x value
  cut    = None
  print(yr)
  for n,one in enumerate(lst):
    xl + one;  yl + one                     # add to left
    xr - one;  yr - one                     # remove from right
    #-------------------------------------------------------------------------
    if xr.n > step:                         # if there is enough on right
        if xl.n >= step:                    # if there is enough of lect
          after = lst[n+1]
          if x( after ) != x( one ):        # if we can break here (since the x values change)
            if x(one) - xlo > tiny:         # if we are more than trivially more than xlo
              if xhi - x(one)  > tiny:      # if we are more than trivially away from xhi
                tmp = yl.xpect(yr)          # what would be the effect of cutting here?
                print(tmp,best)
                if tmp < best:                  # if the effect is good...
                   cut,best = x(lst[n+1]),tmp   # then cutting [0:cut] is the right cut
  return cut,best


def symdiv(lst,
        x=first,     # accessor for the indep variable
        y= last):    # accessor for the dep   variable
  ako  = Num if isNum( y(first(lst)) ) else Sym # how to create the right kind of counter for y
  all={}
  for one in lst:
    if not x(one) in all:
      all[ x(one) ] = ako(key=y, txt=x(one))
    all[ x(one) ] += y(one)
  best = first ( sorted(all,key = lambda z: z.variety()) )
  return best.txt, best.variety()

def div(tbl, y) :
 sorted(tbl.col in tbl.cols.indep

 for col in tbl.my.
'#-------------------------------------------------------
def xtile(lst,lo=The.lo,hi=The.hi,width=The.width,
             chops=[0.1 ,0.3,0.5,0.7,0.9],
             marks=["-" ," "," ","-"," "],
             bar="|",star="*",show=" %3.0f"):
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

if __name__ == "__main__":
 lst=[[1,1], [2,1], [3,1], 
       [4,2], [5,2], [6,2], [7,2], 
       [9,3], [10,3], [11,3], [12,3]] * 10**3
 print(div(lst))
 lst=[[1,1], [2,1], [3,1], 
       [4,1], [5,1], [6,1], [7,1], 
       [9,1], [10,1], [11,1], [12,1]] 
 print(div(lst)) 
