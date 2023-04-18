# vim: set ts=2:sw=2:et:
"""
coc.py: timm
asdas

OPTIONS:
  -h --help asdasasas = False
  -c --cohen saassda  = .95
"""
import re
from ast import literal_eval as coerce
import math
from functools import cmp_to_key
from copy import deepcopy

the={m[1]:coerce(m[2]) 
     for m in re.finditer(r"\n  -[\w]+\s*--([\w]+)[^=]*=\s*([\S]+)",__doc__)}

class thing(object):
  n=0
  def __init__(i, **d): i.__dict__.update(**(i.slots(**d)));  o.n+=1; i.id=o.n
  def slots(i,**d)    : return d
  def __repr__(i)     : return str({k:v for k, v in i.__dict__.items()})
  def __hash__(i)     : return i.id

the=thing(**the)

class NUM(thing):
  def slots(i,at=0,txt=0): 
    return dict(at=at, txt=txt, lo=10**32, hi=-10**32,w= -1 if txt[-1]=="-" else 1)

class SYM(thing):
  def slots(i,at=0,txt=0): return dict(at=at, txt=txt, has={})

class ROW(thing):
  def slots(i,cells=[]): return dict(cells=cells,klass=None,cooked=[])

def COLS(a): 
  all,x,y,klass = [],[],[],None
  for c,x in enumerate(a):
    col = (NUM if x[0].isupper() else SYM)(c,x)
    all += [col]
    if x[-1] != "X":
      (y if x[-1] in ["!","+","-"] else x).append(col)
      if x[-1] == "!":  klass=col
  return thing(names=a, all=all, x=x, y=y, klass=klass)

def DATA(src, inits=[]):
  data = thing(rows=[], cols=None)
  if   type(src)==str  : [add(data,x) for x in csv(src)]
  elif type(src)==list : [add(data,x) for x in src]
  elif type(src)==o    : add(data, src.cols.names)
  [add(data,x) for x in inits] 
  return data 

D=DATA()
print(NUM(at=10,txt="adas-"))
print(SYM(at=10,txt="adas-"))
print(ROW([1,2,3]))
print(DATA())
print(COLS())

exit()
#---------------------------------------------------------------
def csv(f):
  with open(f) as fp:
    for line in fp:
      line = re.sub(r'([\n\t\r"\' ]|#.*)', '', line)
      if line:
         yield [cell.strip() for cell in line.split(",")]

# def data(src):
#   out=DATA()
#   if type(src)==str: 
#     for a in csv(file): add(out,a)
#   else:
#     if type(src)==o:
#       add(a, src.names)
#     if D.names:
#       a = [add(col, a[col.at]) for col in D.all]
#       D.rows += [ROW(a)]
#     else:
#       D.names = a
#       D.all   = [head(c,x) for c,x in enumerate(a)]

def head(c,x):
  col = (NUM if re.match(r"^[A-Z]",x) else SYM)(c,x)
  (D.y if x[-1] in ['-','+','!']  else D.x).append(col) 
  return col
 
def add(col,x,inc=1):
  if x != "?":
    if col.isa == NUM:
      x = float(x)
      col.lo = min(x, col.lo)
      col.hi = max(x, col.hi)
    else:
      col.has[x] = col.has.get(x,0) + inc
  return x

def merge(col1,col2):
  out = deepcopy(col1)
  if out.isa == NUM:
    for x in col2.has: add(out,x)
  else:
    for x,n in col2.has.items(): add(out,x,n)
  return out 
#-------------------------------------------------------------------
def nums(col,rows):
  seen = NUM()
  for row in rows: add(seen, row.cells[col.at])
  return sorted(seen.has)

def syms(col,rows):
  seen = SYM()
  for row in rows: add(seen, row.cells[col.at])
  return seen.has

def mid(col,rows):
  if col.isa == NUM:
    a = nums(col,rows)
    n = len(a)
    return a[int(n/2)]
  else:
    most,mode = -1, None
    for x,n in syms(col.rows).items():
      if n > most:
        most,mode = n,x
    return mode 

def div(col,rows):
  if col.isa == NUM:
    a = nums(col,rows)
    n = len(a)
    return (a[int(n*.9)] - a[int(n*.1)])/2.56
  else:
    e,nall = 0,0
    for x,n in syms(col.rows).items(): nall += n
    for x,n in syms(col.rows).items():
      if n > 0:
        p  = n/nall
        e -= p*math.log(p,2)
    return e 

def norm(num, x):
  return x if x=="?" else (x - num.lo)/(num.hi - num.lo+10**-31)

def stats(rows,cols=D.y,fun=mid):
  out = {col.txt:fun(col,rows) for col in cols}
  out["N"] = len(rows)
  return out

def better(row1,row2):
  s1,s2,cols,n=0,0,D.y,len(D.y)
  for col in cols:
    a,b = norm(col, row1.cells[col.at]), norm(col, row2.cells[col.at])
    s1 -= math.exp(col.w*(a-b)/n)
    s2 -= math.exp(col.w*(b-a)/n)
  return s1/n < s2/n

def betters(rows):
  return sorted(rows, key=cmp_to_key(better))

def bins(rows,x,y):
  rows    = sorted(rows, key=lambda r:r.cells[x.at])
  small   = len(rows)/16
  eps     = div(x,rows) * .35
  out,tmp,seen = [],[],SYM()
  for i,row in enumerate(rows):
    if i > small and i < len(rows) - small:
      if x(row) != x(rows[i+1]):
        if len(tmp) > small:
          if (x(tmp[-1]) - x(tmp[0])) > eps:
             out += [thing(rows=tmp, seen=seen)]
             tmp,seen  = [],SYM()
    tmp += [row]
    add(seen, y(row))
  if tmp: out += [thing(rows=tmp,seen=seen)]
  return merges(out, small)

def merges(bins, small):
  tmp,i = [],0
  while i < len(bins):
    one = bins[i]
    if i < len(bins) - 1:
      two = bins[i+1]
      onetwo = merge(one.seen, two.seen)
      n1,n2  = len(one.rows), len(two.rows)
      if n1 < small or n2 < small or  (
         div(onetwo) <= (n1*div(one) + n2*div(two))/(n1+n2)):
         a = thing(rows = (one.rows + two.rows),
               seen = onetwo)
         j = j + 1
    tmp += [a]
    j = j + 1
  return bins if len(tmp)==len(bins) else bins(tmp,small)

slurp("/Users/timm/gists/data/auto93.csv")
print(stats(D.rows,fun=div))
tmp=betters(D.rows)
for row in tmp[:-30]: row.klass=False
for row in tmp[-30:]: row.klass=True
