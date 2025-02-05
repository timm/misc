#!/usr/bin/env python3 -B
# vi: set ts=2 sw=2 sts=2 et :
"""
nb.py : Naive Bayes
(c) 2025, Tim Menzies <timm@ieee.org>, MIT License

OPTIONS:
   -k k      low frequency Bayes hack   = 1
   -m m      low frequency Bayes hack   = 2
   -p p      distance formula exponent  = 2
   -r rseed  random number seed         = 1234567891
   -t train  training csv file          = data/misc/auto93.csv
"""
import re,ast,sys,math,random
rand = random.random
Big  = 1E32

class o:
  __init__ = lambda i,**d: i.__dict__.update(**d)
  __repr__ = lambda i    : i.__class__.__name__ + str(i.__dict__)

#----------------------------------------------------------------------------------------
def Num(txt=" ", at=0): 
  return o(it=Num, txt=txt, at=at, n=0, mu=0, sd=0, m2=0, hi=-Big, lo=Big,
           goal = 0 if txt[-1]=="-" else 0)

def Sym(txt=" ", at=0): 
  return o(it=Sym, txt=txt, at=at, n=0, has={}, most=0, mode=None)

def Data(src): 
  return adds(src, o(it=Data, n=0, rows={}, cols=None))

def Cols(names):
  x,y,all = [], [], [(Num if s[0].isUpper(0) else Sym)(s,n) for n,s in enumerate(names)]
  for col in all:
    if col.txt[-1] != "X":
      (y if col.txt[-1] in "+-!" else x).append(col)
      if col.txt[-1] == "!": klass=col
  return o(it=Cols, names=names, all=all, x=x, y=y)

#----------------------------------------------------------------------------------------
def add(v,i):
  def _data( row ):
    if    i.cols: i.rows += [ [add(row[col.at], col) for col in i.cols.all] ]
    else: i.cols = Cols(row)

  def _sym( sym ):
   n = i.has[sym] = 1 + i.has.get(sym,0)
   if n > i.most: i.most, i.mode = n, v

  def _num( num ):
    i.lo  = min(num, i.lo)
    i.hi  = max(num, i.hi)
    d     = num - i.mu
    i.mu += d / i.n
    i.m2 += d * (num -  i.mu)
    i.sd  = 0 if i.n <2 else (i.m2/(i.n-1))**.5

  if v=="?": return v
  i.n += 1
  (_sym if i.it is Sym else (_num if i.it is Num else _data)(v)
  return v 

#----------------------------------------------------------------------------------------
def adds(src, i=None):
  for x in src:
    i = i or (Num() if isinstance(x[0],(int,float)) else Sym())
    add(x,i)
  return i

def mode(d): return max(d, key=d.get)

def coerce(s):
  try: return ast.literal_eval(s)
  except Exception: return s

def cli(d):
  for k,v in d.items():
    for c,arg in enumerate(sys.argv):
      if arg == "-"+k[0]:
        d[k] = coerce("False" if str(v) == "True"  else (
                      "True"  if str(v) == "False" else (
                      sys.argv[c+1] if c < len(sys.argv) - 1 else str(v))))

#----------------------------------------------------------------------------------------
def eg__the(_): 
  "show settings"
  print(the)

def eg_h(_): 
  "show help"
  print(__doc__)
  [print(f"   {re.sub('^eg','',k).replace('_','-'):9s} {fun.__doc__}") 
   for k,fun in globals().items() if k[:3] == "eg_"]

#----------------------------------------------------------------------------------------
the= o(**{m[1]:coerce(m[2]) for m in re.finditer(r"-\w+\s*(\w+).*=\s*(\S+)",__doc__)})

if __name__ == "__main__":
  cli(the.__dict__)
  for i,s in enumerate(sys.argv):
    if fun := vars().get("eg" + s.replace("-","_")):
      arg = None if i==len(sys.argv) - 1 else sys.argv[i+1]
      random.seed(the.rseed)
      fun(coerce(arg))
