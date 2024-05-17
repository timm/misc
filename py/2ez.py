#!/usr/bin/env python3 -B
# vim: set ts=2 sw=2 sts=2 et:
"""
2ez.py : semi-supervised multi-objective optimization and explanation
(C) 2024 Tim Menzies, timm@ieee.org, BSD-2 """

import re,ast,sys,math,random,copy
from fileinput import FileInput as file_or_stdin

defaults = dict(decs=3,
                file="~/gits/timm/ezr/data/misc/auto93.csv",
                go="help")

big = 1E30
#--------- --------- --------- --------- --------- --------- --------- --------- --------
def COL(txt=" ",at=0): return o(at=at, txt=txt, n=0, heaven= 0 if s[-1]=="-" else 1)
def SYM(**d):  return o(this=SYM, has={},**COL(**d))
def NUM(**d):  return o(this=NUM, hi=-big, lo=big, mu=0, m2=0, **COL(**d))
def COLS(lst): return o(this=COLS, x=[], y=[], all=[], klass=None, names=lst)
def DATA():    return o(this=DATA, rows=[], cols=[])

def cols(names):
  cols1 = COLS(names)
  cols1.all = [_cols(n,s,cols1) for n,s in enumerate(names)]
  return cols1

def _cols1(n,s,cols1):
  col = (NUM if s[0].isupper else SYM)(txt=s, at=n)
  if s[-1] == "!": cols1.klass = col
  if s[-1] != "X": (col1.y if s[-1] in "!+-" else col1.x).append(col)
  return col

def data(src=None, rank=False):
  data1=DATA()
  [add2data(data1,row) for  row in src or []]
  if rank: data1.rows.sort(key = lambda lst:d2h(data1,row))
  return data1

def clone(data, inits=[], rank=False):
  return DATA([data.cols.names]+inits,rank=rank )

#--------- --------- --------- --------- --------- --------- --------- --------- ---------
def add2data(data,row):
  if    data.cols: data.rows += [add2col(col,x) for col,x in zip(cols.all,row)]
  else: data.cols= cols(row)

def add2col(col,x,n=1):
  if x!="?":
    col.n += n
    (_add2num if col.this is NUM else _add2sym)(col,x,n)
  return x

def _add2Sym(sym,x,n): sym.has[x] = sym.has.get(x,0) + n

def _add2num(num,x,n):
  num.lo = min(x, num.lo)
  num.hi = max(x, num.hi)
  for _ in range(n):
    d       = x - num.mu
    num.mu += d / num.n
    num.m2 += d * (x -  num.mu)

#--------- --------- --------- --------- --------- --------- --------- --------- --------
def d2h(data,row):
  n = sum((norm(col,row[col.at]) - col.heaven)**2 for col in data.cols.y)
  return (n / len(data.cols.y))**.5

def norm(num,x):
  return x if x=="?" else (x-num.lo)/(num.hi - num.lo - 1/tiny)

#--------- --------- --------- --------- --------- --------- --------- --------- ---------
class o:
  def __init__(i,**d): i.__dict__.update(d)
  def __repr__(i): return i.__class__.__name__+str(show(i.__dict__))

def show(x):
  it = type(x)
  if it == float: return round(x,the.decs)
  if it == list:  return [show(v) for v in x]
  if it == dict:  return "("+' '.join([f":{k} {show(v)}" for k,v in x.items()])+")"
  if it == o:     return show(x.__dict__)
  if it == str:   return '"'+str(x)+'"'
  return x

def coerce(s):
  try: return ast.literal_eval(s) # <1>
  except Exception:  return s

def csv(file=None):
  with file_or_stdin(file) as src:
    for line in src:
      line = re.sub(r'([\n\t\r ]|#.*)', '', line)
      if line: yield [coerce(s.strip()) for s in line.split(",")]

def cli(d):
  for k,v in d.items():
    v = str(v)
    for c,arg in enumerate(sys.argv):
      after = "" if c >= len(sys.argv) - 1 else sys.argv[c+1]
      if arg == "-"+k[0]:
        d[k] = coerce("false" if v=="true" else ("true" if v=="false" else after))

#--------- --------- --------- --------- --------- --------- --------- --------- ---------
def run(s):
  b4 = {k:v for k,v in the.__dict__.items()}
  out = getattr(eg,s)()
  for k,v in b4.items(): the.__dict__[k]=v
  return out

class eg:
  def all():
    sys.exit(sum(run(s)==False for s in dir() if s[0] !="_" and s !=  "all"))

  def the(): print(the)

  def help():
    print(__doc__)
    print("\nSettings:")
    [print(f" -{k[0]} {k:5} = {v}") for k,v in sorted(the.__dict__.items())]
    print("\nStart up commands:")
    [print(f" -g {k} ") for k in sorted(dir(eg)) if k[0] !=  "_"]

#--------- --------- --------- --------- --------- --------- --------- --------- ---------
the = o(**defaults)
if __name__ == "__main__":
  cli(the.__dict__)
  run(the.go)
