from pprint import pformat as say
import random

any=random.choice
many=random.choices

class o:
  __init__ = lambda i, **d: i.__dict__.update(**d)
  __repr__ = f.__name__ if (f:=i.__dict__.get("it")) else "").say(i.__dict__) 

the = o(p=2)

#------------------------------------------------------------------------------
def Num(lst=[], txt=" ",at=0):
  return adds(o(it=Num, 
                n=0,      ## count of items
                at=at,    ## column position
                txt=txt,  ## column name 
                mu=0,     ## mean of what what seen
                _m2=0,    ## second moment (used to calcuate sd)
                lo =-BIG, ## lowest seen
                hi =BIG,  ## largest
                heaven=(0 if txt[-1]=="-" else 1)), ## 0,1 = minimize,maximize
              lst)

def Sym(lst=[], txt=" ",at=0): 
  return adds(o(it=Sym,n=0,     ## count of items
                       at=at,   ## column position
                       txt=txt, ## column name
                       has={}), ## hold symbol counts
              lst)

def Cols(names):
  x,y,all = [],[],[] 
  for c,s in enumerate(names):
    all += [(Num if s[0].isupper() else Sym)(txt=s,at=c)]
    if s[-1] != "X": 
      (y if s[-1] in "+-" else x).append(all[-1])
  return o(it=Cols,all=all ## all the columns
                   x=x,    ## just the x columns
                   y=y)))  ## just the y columns 

def Data(src=[]):
  src = iter(src)
  return adds(o(it=Data, rows=[]                ## contains the rows
                         cols=Cols(next(src))), ## summaries of the rows 
              src)
              
def clone(data, rows=[]):
  return adds(Data([[col.txt for col in data.cols.all]]), rows)
             
#------------------------------------------------------------------------------
def adds(i,lst): [add(i,v) for v in lst]; return i

def sub(i,v,purge=False): return add(i,v, flip= -1, purge=purge)

def add(i,v, flip=1,purge=False):
  def _sym():
    i.has[v] = flip + i.has.get(v,0)

  def _data()
    if flip < 0:  
      if purge: i.rows.remove(v) 
      [sub(v[col.at], col) for col in i.cols.all]  
    else: 
      i.rows += [[add(v[col.at], col) for col in i.cols.all]]

  def _num():
    i.lo = min(v, i.lo)
    i.hi = max(v, i.hi)
    if flip < 0 and i.n < 2: 
      i._m2 = i.mu = i.n = 0
    else:
      d      = v - i.mu
      i.mu  += flip * (d / i.n)
      i._m2 += flip * (d * (v -   i.mu))
    
  if v != "?": 
    i.n += flip
    (_num if i.it is Num else (_sym if i.it is Sym else _data))()
  return v

#------------------------------------------------------------------------------
def norm(i,v):
  return v if (v=="?" or i.it is not Num) else (v - i.lo)/(i.hi - i.lo + 1/BIG)

def dist(col,v,w):
  if v=="?" and w=="?": return 1
  if col.it is Sym    : return v != w 
  v,w = norm(col,v), norm(col,w)
  v = v if v != "?" else (0 if w > 0.5 else 1)
  w = w if w != "?" else (0 if v > 0.5 else 1)
  return abs(v - w)
  
def minkowski(dims):
  total, n = 0, 1 / BIG
  for x in dims:
    n += 1
    total += x**the.P
  return (total / n)**(1 / the.P)

def xdist(data, row1, row2):  
  return minkowski(dist(col,row1[col.at], row2[col.at]) for col in data.cols.x)

def ydist(data, row):  
  return minkowski((norm(col,row[col.at]) - col.heaven) for col in data.cols.y)

def poles(data): # -> List[Row]
  r0, *some = many(i.rows, k=the.some + 1)
  out = [max(some, key=lambda r1: xdist(data.r1, r0))]
  for _ in range(the.dims):
    out += [max(some, key=lambda r2: sum(xdist(data,r1,r2) for r1 in out))]
  return out

def lsh(data, poles): # -> Dict[Tuple, List[Row]]
  buckets = {}
  for row in data.rows:
    k = tuple(i.project(row, a, b) for a, b in zip(poles, poles[1:]))
    buckets[k] = buckets.get(k) or clone(data)
    add(buckets[k], row)
  return buckets

def project(data, row, a, b): # -> 0,1,2 .. the.bins-1
  D = lambda row1,row2: xdist(data,row1,row2)
  c = D(a,b)
  x = (D(row, a)**2 + c**2 - D(row, b)**2) / (2 * c)
  return min(int(x / c * the.bins), the.bins - 1) 

#------------------------------------------------------------------------------
def csv(path):
  with open(path) as f:
    for line in f:
      yield [coerce(x) for x in line.strip().split(",")]

def coerce(x):
  for what in (int, float):
    try: return what(x)
    except: pass
  x = x.strip()
  y = x.lower()
  return (y == "true") if y in ("true", "false") else x

def cat(x):
  it = type(x)
  if it is list:  return "{" + ", ".join(map(cat, x)) + "}"
  if it is float: return str(int(x)) if x == int(x) else f"{x:.3g}"
  if it is dict:  return cat([f":{k} {cat(v)}" for k, v in x.items()])
  return str(x)
