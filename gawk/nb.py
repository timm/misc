#!/usr/bin/env python3
"""
nb.py: naive bayes classifier
(c) 2026, Tim Menzies, MIT license.

USAGE
   python3 nb.py [OPTIONS] [FILE]

DESCRIPTION
    Incremental naive bayes. Training and testing are interleaved: after
    burn-in, each row is classified then added to the training set.

OPTIONS
    -h          Show help.
    -k k=1      Bayes low frequency hack for symbolic attributes.
    -m m=2      Bayes low frequency hack for class priors.
    -w wait=5   Start classifying after seeing "some" rows

EXAMPLES
    --the       Print config settings.
    --sym       Test symbolic column.
    --num       Test numeric column.
    --csv F     Print rows from CSV file.
    --nb F      Run naive bayes on CSV file.

INPUT FORMAT
    Comma-separated values. First row defines column names. Uppercase
    names (Age, Weight) are numeric; lowercase (name, color) are symbolic.
    Suffixes: "!" class label, "X" ignore.
    Missing values: "?".

--------------------------------------------------------------------------
CODING STANDARD

  Type Hints (single letter)
    i:instance(Obj)   t:target(dict)   s:string    n:number
    r:row(list)       c:col(Obj)       v:value     f:file/filename
    d:delta/data      k:class/key      b4:before(prior)

  Class System
    Obj(dict):        Base class, provides dot notation access (d.x).
    CamelCase(args):  Factory functions (Sym, Num, Data) returning Obj.

  Function Signatures
    Functional style is preferred over method chaining:
      add(i, v)       -- update object i with value v

--------------------------------------------------------------------------
API

  # Constructors
  Sym(n=0, s="")      -- Create symbolic column at position n, name s.
  Num(n=0, s="")      -- Create numeric column at position n, name s.
  Data(s="", items=[])-- Create dataset from list of rows/items.
  clone(d, rows=[])   -- Create new Data with same structure as d.
  Cols(row)           -- Generate column headers from a list of names.

  # Classifier
  nb(items)           -- Run incremental Naive Bayes on item iterator.

  # Methods (Functional)
  add(i, v)           -- Update counts (Sym) or Welford stats (Num).
                      -- If i is Data, add row and update cols.
  like(i, v, prior)   -- Calculate likelihood of v given column i.
  likes(i, r, nall, nh)-- Calculate log-likelihood of row r given Data i.

  # Utilities
  coerce(s)           -- Parse string to int, float, or strip whitespace.
  csv(file)           -- Iterator yielding rows from CSV file.
  o(t)                -- Pretty print object/dict t.
"""

import re, sys, math
from math import sqrt, exp, log
BIG = 1E32
the={}

# --- Obj ---------------------------------------------------------------
def o(t):
  if isinstance(t, dict): 
    t = t.__class__.__name__+"{"+", ".join(f":{k} {o(v)}" 
                                           for k,v in t.items())+"}"
  return str(t) if not isinstance(t, float) else f"{t:.2f}"

class Obj(dict):
  __getattr__,__setattr__,__repr__ = dict.__getitem__,dict.__setitem__,o

# --- create ------------------------------------------------------------
def Sym(n=0, s=""): return Obj(at=n, txt=s, n=0, syms={})
def Num(n=0, s=""): return Obj(at=n, txt=s, n=0, mu=0, m2=0, sd=0)

def Data(s="", items=[]):
  d = Obj(txt=s, rows=[], cols=None)
  [add(d, r) for r in items]
  return d

def Cols(row):
  all = [(Num if s[0].isupper() else Sym)(n,s)for n,s in enumerate(row)]
  return Obj(names=row, all=all,
             x=[c for c in all if not re.search(r"[!X]$", c.txt)],
             y=[c for c in all if re.search(r"!$", c.txt)])

def clone(d, rows=[]): return Data(d.txt, [d.cols.names] + rows)

# --- update ------------------------------------------------------------
def add(i, v):
  if v != "?":
    if "rows" in i:
      if not i.cols: i.cols = Cols(v)
      else: i.rows.append([add(c, v[c.at]) for c in i.cols.all])
    else:
      i.n += 1
      if "syms" in i: i.syms[v] = 1 + i.syms.get(v, 0)
      else:
        d = v - i.mu; i.mu += d/i.n; i.m2 += d*(v - i.mu) # welford
        i.sd = 0 if i.n < 2 else (i.m2/(i.n - 1))**.5
  return v

 # --- bayes -------------------------------------------------------------
def like(i, v, prior=0):
  if "syms" in i:   # Sym
    n = i.syms.get(v, 0) + the["k"]*prior
    return max(1/BIG, n/(i.n + the["k"] + 1/BIG))
  else:             # Num
    var = i.sd**2 + 1/BIG
    return (1/sqrt(2*math.pi*var)) * exp(-((v - i.mu)**2)/(2*var))

def likes(i, r, nall, nh):
  b4 = (len(i.rows) + the["m"])/(nall + the["m"]*nh)
  return log(b4) + sum(log(like(c, r[c.at], b4)) 
                       for c in i.cols.x if r[c.at] != "?")

def nb(items: list[list]):
  all, klasses, nk, out = None, {}, 0, Sym()
  for n, r in enumerate(items):
    if not all: all = Data("all", [r])
    else:
      k = r[all.cols.y[0].at]
      if k not in klasses: nk += 1; klasses[k] = clone(all)
      if (n - 1) > the["wait"]: 
        pred = max(klasses, key=lambda cat: likes(klasses[cat],r,n-1,nk))
        add(out, (pred, k)) 
      add(klasses[k], r)
  return out
 
# --- lib ---------------------------------------------------------------
def coerce(s):
  try: return int(s)
  except:
    try: return float(s)
    except: return s.strip()

def csv(file):
  with open(file) as f:
    for s in f: yield [coerce(x) for x in s.split(",")]

# --- main --------------------------------------------------------------
eg = {}
eg["-h"]     = lambda _: print(help)
eg["--the"]  = lambda _: print(o(the))
eg["--sym"]  = lambda _: print(add(add(add(Sym(),"a"),"a"),"b"))
eg["--num"]  = lambda _: print([add(Num(), x) for x in [10,20,30,40]][-1])
eg["--csv"]  = lambda f: [print(r) for r in csv(f)]
eg["--nb"]   = lambda f: [print(n,*x) for x,n in nb(csv(f)).syms.items()] 

for k,v in re.findall(r"(\S+)=(\S+)", help): the[k] = coerce(v)
if __name__ == "__main__":
  for j,s in enumerate(sys.argv):
    if s in eg: eg[s](sys.argv[j+1] if j+1 < len(sys.argv) else None)
