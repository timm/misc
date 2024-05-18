#!/usr/bin/env python3 -B
# vim: set ts=2 sw=2 sts=2 et:

"""
2ez.py : semi-supervised multi-objective optimization and explanation
(C) 2024 Tim Menzies, timm@ieee.org, BSD-2
"""

import re,ast,sys,math,random,copy
from fileinput import FileInput as file_or_stdin

class o:
  def __init__(i,**d): i.__dict__.update(d)
  def __repr__(i): return i.__class__.__name__+str(show(i.__dict__))

the = o(decs  = 3,
        file  = "../../ezr/data/misc/auto93.csv",
        go    = "help",
        k     = 1,
        m     = 2,
        n     = 12,
        N     = 0.5,
        start = 4,
        Stop  = 20,
        top   = 0.8)

big = 1E30
#--------- --------- --------- --------- --------- --------- --------- --------- --------
def DATA():            return o(DATA, rows=[], cols=[])
def COLS(lst):         return o(x=[], y=[], all=[], klass=None, names=lst)
def SYM(txt=" ",at=0): return o(isNum=False, txt=txt, at=at, n=0, has={})
def NUM(txt=" ",at=0): return o(isNum=True,  txt=txt, at=at, n=0, hi=-big, lo=big, 
                                             mu=0, m2=0, heaven= 0 if txt[-1]=="-" else 1)

def cols(names):
  cols1 = COLS(names)
  cols1.all = [_cols(cols1,n,s) for n,s in enumerate(names)]
  return cols1

def _cols(cols1, n, s):
  col = (NUM if s[0].isupper else SYM)(txt=s, at=n)
  if s[-1] == "!": cols1.klass = col
  if s[-1] != "X": (cols1.y if s[-1] in "!+-" else cols1.x).append(col)
  return col

def data(src=None, rank=False):
  data1=DATA()
  [add2data(data1,row) for  row in src or []]
  if rank: data1.rows.sort(key = lambda row:d2h(data1,row))
  return data1

def clone(data, inits=[], rank=False):
  return DATA([data.cols.names]+inits,rank=rank )

#--------- --------- --------- --------- --------- --------- --------- --------- ---------
def add2data(data,row):
  if    data.cols: data.rows += [[add2col(col,x) for col,x in zip(data.cols.all,row)]]
  else: data.cols= cols(row)

def add2col(col,x,n=1):
  if x!="?":
    col.n += n
    (_add2num if col.isNum else _add2sym)(col,x,n)
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
def mid(col): 
  return col.mu if col.isNum else max(col.has, key=col.has.get)

def mids(data, cols=None): 
  return {col.txt:mid(col) for col in cols or data.cols.x}

def div(col): 
  return  (0 if col.n <2 else (col.m2/(col.n-1))**.5) if cols.isNum else ent(col.has)

def divs(data, cols=None): return {col.txt:div(col) for col in cols or data.cols.x}

#--------- --------- --------- --------- --------- --------- --------- --------- --------
def d2h(data,row):
  n = sum((norm(col,row[col.at]) - col.heaven)**2 for col in data.cols.y)
  return (n / len(data.cols.y))**.5

def norm(num,x): return x if x=="?" else (x-num.lo)/(num.hi - num.lo - 1/big)

#--------- --------- --------- --------- --------- --------- --------- --------- --------
def loglikes(data, row, nall, nh):
  prior = (len(data.rows) + the.k) / (nall + the.k*nh)
  likes = [like(col,row[col.at],prior) for col in data.cols.x if row[col.at] != "?"]
  return sum(math.log(x) for x in likes + [prior] if x>0)

def like(col, x, prior): 
  return like4num(col,x) if col.isNum else like4sym(col,x,prior)
  
def like4sym(sym,x,prior): return (i.has.get(x, 0) + the.m*prior) / (i.n + the.m)

def like4num(num,x):
  v     = div(num)**2 + 1/big
  nom   = math.e**(-1*(x - mid(num))**2/(2*v)) + 1/big
  denom = (2*math.pi*v)**.5
  return min(1, nom/(denom + 1/big))

#--------- --------- --------- --------- --------- --------- --------- --------- --------
def smo(data, score=lambda B,R: B-R):
  def guess(row, best, rest, n): 
    return score(loglikes(best, row, n, 2), loglikes(rest, row, n, 2))

  def guesses(todo, done):
    n    = len(done)
    cut  = int(n ** the.N) 
    top  = int(n *  the.top)
    best = clone(data, done[:cut])
    rest = clone(data, done[cut:])
    return sorted(todo, key=lambda row: guess(row, best, rest, n))[:top]

  def smo1(todo, done):
    for _ in range(the.Stop - the.start):
      if len(todo) < 3: break
      top,*todo = guesses(todo, done)
      done += [top]
      done = clone(data, done, rank=True).rows # done is now resorted
    return done[0]

  random.shuffle(data.rows)
  return smo1(data.rows[the.start:], clone(data.rows[:the.start], rank=True).rows)
   
#--------- --------- --------- --------- --------- --------- --------- --------- ---------
def ent(d):
  N = sum(v for v in d.values() if v > 0)
  return -sum(v/N*math.log(v/N,2) for v in d.values() if v > 0)

def show(x):
  it = type(x)
  if it == float:  return round(x,the.decs)
  if it == list:   return [show(v) for v in x]
  if it == dict:   return "("+' '.join([f":{k} {show(v)}" for k,v in x.items()])+")"
  if it == o:      return show(x.__dict__)
  if it == str:    return '"'+str(x)+'"'
  if callable(it): return "()"
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

def green(s):  return re.sub(r"^(...)", r"\033[92m\1\033[00m",s)
def yellow(s): return re.sub(r"(.*)", r"\033[93m\1\033[00m",s)
def cyan(s):   return re.sub(r"(.*)", r"\033[96m\1\033[00m",s)

#--------- --------- --------- --------- --------- --------- --------- --------- ---------
def run(s):
  b4 = {k:v for k,v in the.__dict__.items()}
  out = getattr(eg, s)()
  for k,v in b4.items(): the.__dict__[k]=v
  return out

class eg:
  def all(): sys.exit(sum(run(s)==False for s in dir() if s[0] !="_" and s !=  "all"))

  def the(): print(the)

  def csv(): [print(x) for x in csv(the.file)]

  def data(): 
    data1= data(src = csv(the.file), rank=True)
    print(show(mids(data1, cols=data1.cols.y)))
    print(data1.cols.names)
    for i,row in enumerate(data1.rows):
      if i % 50 == 0: print(row)

  def loglike():
    data1= data(src = csv(the.file))
    for i,row in enumerate(data1.rows):
      if i % 50 == 0: print(loglike(data1, row, 500, 2))

  def help():
    print(cyan(f"{__doc__}"))
    print(yellow(f"\nSettings:"))
    [print(green(f" -{k[0]} {k:5} = {v}")) for k,v in the.__dict__.items()]
    print(yellow(f"\nStart-up commands:"))
    [print(green(f" -g {k} ")) for k in sorted(dir(eg)) if k[0] !=  "_"]

#--------- --------- --------- --------- --------- --------- --------- --------- ---------
if __name__ == "__main__":
  cli(the.__dict__)
  run(the.go)
