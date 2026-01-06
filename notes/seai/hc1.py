#!/usr/bin/env python3 -B
import random, sys, xai
from xai import distx as x, disty as y, shuffle, Data, csv,the

xai.the.data=sys.argv[2]
random.seed(int(sys.argv[1]))
data = Data(csv(the.data))

def Y(r): return y(data, r)

def extremes(rows):
  rows = sorted(rows, key=Y)
  n = len(rows)//10
  return rows[n], rows[9*n]

def project(r, ok, no):
  c= x(data,ok,no)
  return (x(data,r,ok)**2 + c*c - x(data,r,no)**2)/(2*c + 1e-32)

def prune(rows, ok, no):
  return sorted(rows, key=lambda r: project(r, ok, no))[:len(rows)//2]

def report(rows):
  ys = sorted(Y(r) for r in rows)
  print(round(ys[0],2), end=" ")

def mu(lst):
  tmp=sorted(lst)
  n = len(tmp)//10
  return (tmp[9*n] - tmp[n])/2.56

def mid(a): a.sort(); n=len(a)//10; return a[5*n]
def sd(a) : a.sort(); n=len(a)//10; return (a[9*n] - a[n])/2.56

cohen=0.35
budget=100
step=5
fn=sd
eps = fn([Y(r) for r in data.rows])*cohen
print(round(eps,2))
for _ in range(20):
  labelled, rows = [], shuffle(data.rows[:])
  b4 = 1e32
  report(rows)
  while len(labelled) < budget:
    labelled += shuffle(rows)[:step]
    ok, no = extremes(labelled)
    rows = prune(rows, ok, no) # reduce to best half
    now = fn([Y(r) for r in labelled])
    report(labelled)
    if abs(b4 - now) > eps:
      b4 = now 
    else:
      break
  print(",", len(labelled),end=", "); report(labelled);print("")
