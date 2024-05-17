#!/usr/bin/env python3 -B
# vim : set ts=2 sw=2 et :
import re,ast,sys,math,random,copy
from typing import Any,Iterable,Callable
from fileinput import FileInput as file_or_stdin

big = 1E30

defaults = dict(decs=3,
                file="~/gits/timm/ezr/data/misc/auto93.csv",
                go="load")

def COL(s=" ",at=0): return (NUM if s[0].isupper else SYM)(s=s, at=at)
def SYM(s=" ",at=0): return OBJ(isNum=0, txt=s, at=at, n=0, seen={})
def NUM(s=" ",at=0): return OBJ(isNum=1, txt=s, at=at, n=0, hi=-big, lo=big, mu=0,
                                heaven= 0 if s[-1]=="-" else 1)

def COLS(lst):
  cols = OBJ(x=[], y=[], all=[COL(s=txt,at=n) for n,s in lst.items()])
  for col in self.all: 
    (cols.y if col.txt[-1] in "+-!" else cols.x).append(col)
  return cols

def DATA(lsts, rank=False):
  data = OBJ(rows=[],col=[])
  for row in lsts: addRow(data,row)
  if rank: data.rows.sort(key = lambda lst:d2h(data,row))
  return data

def d2h(data,row):
  n = sum((norm(col,row[col.at]) - col.heaven)**2 for col in data.cols.y)
  return (n / len(data.cols.y))**.5
 
def norm(num,x):
  return x if x=="?" else (x-num.lo)/(num.hi - num.lo - 1/tiny)

#------------------------------------------------------------------------------
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

class OBJ:
  def __init__(i,**d)    : i.__dict__.update(d)
  def __repr__(i) -> str : 
    d = {k:round(v,the.decs) if type(v)==float else v for k,v in i.__dict__.items()}
    return i.__class__.__name__+str(d)

#------------------------------------------------------------------------------
def run(s):
  b4 = {k:v for k,v in the.__dict__.items()}
  getattr(eg,s)()
  for k,v in b4.items(): the.__dict__[k]=v

class eg:
  def load(): print(1)
    
#------------------------------------------------------------------------------
the = OBJ(**defaults)
if __name__ == "__main__":
  cli(the.__dict__)
  run(the.go)
