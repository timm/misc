#!/usr/bin/env python3 -B
import random, sys, xai
from xai import distx as x
from xai import disty as y
from xai import shuffle,Data,csv

xai.the.data=sys.argv[2]
random.seed(int(sys.argv[1]))

data = Data(csv(xai.the.data))

def Y(r): return round(y(data,r),2)

def report(rows): 
    a=sorted(rows[:],key=Y)
    n=len(a)//10
    sd = (Y(a[9*n]) - Y(a[n]))/2.56
    print( f"{Y(a[0]):2}, {sd:.2f}", end=" ")
    return sd*0.35

for _ in range(20):
  labelled, rows = [], shuffle(data.rows)
  eps= report(rows)
  last=1e32
  while len(labelled) < 30:
    labelled = labelled + shuffle(rows)[:8]
    labelled.sort(key=Y)
    n = len(labelled)//10
    ok,no = labelled[n], labelled[9*n]
    #ok,no = labelled[0], labelled[-1]
    c= x(data,ok,no)
    #d= lambda r: (x(data,r,ok)
    d= lambda r: (x(data,r,ok)**2 + c*c - x(data,r,no)**2)/(2*c + 1e-32)
    rows = sorted(rows,key=d)[:len(rows)//2]

  report(labelled)
  print("")
