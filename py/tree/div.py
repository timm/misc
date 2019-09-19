#!/usr/bin/env python3
# vim : nospell filetype=py ts=2 sw=2 sts=2  et  :


from thing import Sym,Num
from the import THE
from lib import *
from copy import deepcopy as kopy

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

if __name__ == "__main__":
 lst=[[1,1], [2,1], [3,1], 
       [4,2], [5,2], [6,2], [7,2], 
       [9,3], [10,3], [11,3], [12,3]] * 10**3
 print(div(lst))
 lst=[[1,1], [2,1], [3,1], 
       [4,1], [5,1], [6,1], [7,1], 
       [9,1], [10,1], [11,1], [12,1]] 
 print(div(lst)) 
