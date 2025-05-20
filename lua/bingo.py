# This code reads csv data from `the.file`, the divides that data into 
# `the.b` bins along `the.D` random projections. After  randomly scoring 
# `the.A` bins, then `the.B` times, it selects two labeled examples, 
# it guesses y-values via extrapolation, and labels the best guess.
# Finally, `the.C` items from the top bin are labeled for evaluation.
#     
# ### In this code:
# - `_` marks private vars/methods
# - `i` means `self`
# - `the` is config, parsed from top docstring (can be updated via CLI)
# - `eg__xxx` are CLI demos (run with `--xxx`)
# - structs use `struct.it` to denote type
# - no classes (so polymorphic methods stay close together)
"""
binlure.py: stochastic landscape analysis for multi-objective reasoning
(c) 2025 Tim Menzies, <timm@ieee.org>. MIT license

Options:
  
  -d d    number of dimensions = 4
  -p p    minkowski co-effecient = 2
  -A A    rows labelled at random duiring code start           = 4
  -B B    rows labelled while reflection of labels seen so far = 30
  -C C    rows labels while testing the supposed best bin      = 5
  -f file csv file for data = ../../moot/optimize/misc/auto93.csv
 """
from pprint import pformat as say
import random

any=random.choice
many=random.choices

# Basic struct with names fields and pretty prints
class o:
  __init__= lambda i, **d: i.__dict__.update(**d)
  __repr__= lambda i: \
              (f.__name__ if (f:=i.__dict__.get("it")) else "").say(i.__dict__) 

### Create -------------------------------------------------------------------
# Summarize a stream of numbers
def Num(lst=[], txt=" ",at=0): # -> Num
  return adds(o(it=Num, 
                n=0,      # count of items
                at=at,    # column position
                txt=txt,  # column name 
                mu=0,     # mean of what what seen
                _m2=0,    # second moment (used to calcuate sd)
                lo =-BIG, # lowest seen
                hi =BIG,  # largest
                heaven=(0 if txt[-1]=="-" else 1)), # 0,1 = minimize,maximize
              lst)

# Summarize a stream of symbols
def Sym(lst=[], txt=" ",at=0):  # -> Sym
  return adds(o(it=Sym,n=0,     # count of items
                       at=at,   # column position
                       txt=txt, # column name
                       has={}), # hold symbol counts
              lst)

# Turn column names into `Num`s and `Sym`s (if upper case, then `Num`).
def Cols(names): # -> Cols
  all,x,y = [],[],[]  
  for c,s in enumerate(names):
    all += [(Num if s[0].isupper() else Sym)(txt=s,at=c)]
    if s[-1] != "X": # what to ignore 
      (y if s[-1] in "+-" else x).append(all[-1])
  return o(it=Cols,all=all, # all the columns
                   x=x,     # just the x columns
                   y=y)     # just the y columns 

# Keep some `rows`, sumamrize them in the `cols`.
def Data(src=[]): # -> Data
  src = iter(src)
  return adds(o(it=Data, rows=[],               # contains the rows
                         cols=Cols(next(src))), # summaries of the rows 
              src)
              
# Mimic the structure of an existing `Data`. Optionally, add some rows.
def clone(data, rows=[]): # -> Data
  return adds(Data([[col.txt for col in data.cols.all]]), rows)
             
### Update --------------------------------------------------------------------
# Update `i` with  multiple things. 
def adds(i,lst): # -> i
  [add(i,v) for v in lst]; return i

# `sub`tracting is just `add`ing -1.
def sub(i,v,purge=False): # -> v
  return add(i,v, flip= -1, purge=purge)

# If `v` is unknown, then ignore. Else, udpate.
def add(i,v, flip=1,purge=False): # -> v
  def _sym():
    i.has[v] = flip + i.has.get(v,0)

  def _data():
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
  if v=="?" and w=="?": 
    return 1
  elif col.it is Sym: 
    return v != w 
  else:
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

def ydist(data, row):  
  return minkowski(abs(norm(c,row[c.at]) - c.heaven) for c in data.cols.y)

def xdist(data, row1, row2):  
  return minkowski(dist(c,row1[c.at], row2[c.at]) for c in data.cols.x)


def project(data, row, a, b): # -> 0,1,2 .. the.bins-1
  D = lambda row1,row2: xdist(data,row1,row2)
  c = D(a,b)
  if c==0: return 0
  return (D(row, a)**2 + c**2 - D(row, b)**2) / (2 * c *c)

def bucket(data,row,a,b):
  return min(int( project(data,row,a,b) * the.bins), the.bins - 1)

def extrapolate(data,row,a,b):
  ya, yb = ydist(data,a), ydist(data,b)
  return ya + project(data,row,a,b) * (yb - ya)  

def poles(data): # -> List[Row]
  r0, *some = many(i.rows, k=the.some + 1)
  out = [max(some, key=lambda r1: xdist(data.r1, r0))]
  for _ in range(the.dims):
    out += [max(some, key=lambda r2: sum(xdist(data,r1,r2) for r1 in out))]
  return out

def lsh(data, poles): # -> Dict[Tuple, List[Row]]
  buckets = {}
  for row in data.rows:
    k = tuple(bucket(row, a, b) for a, b in zip(poles, poles[1:]))
    buckets[k] = buckets.get(k) or clone(data)
    add(buckets[k], row)
  return buckets
  
def neighbors(c, hi):
  def go(i, p):
    if i == len(c):
      t = tuple(p)
      if t != c and all(0 <= x < hi for x in t): yield t
    else:
      for d in [-1, 0, 1]:
        yield from go(i+1, p+[c[i]+d])
  return go(0, [])

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

### Start-up ------------------------------------------------------------------
def cli(d: Dict[str, Any]) -> None:
  "LIB: Process command line arguments."
  for k, v in d.items():
    for c, arg in enumerate(sys.argv):
      if arg == "-" + k[0]:
        d[k] = coerce("False" if str(v) == "True" else (
                      "True" if str(v) == "False" else (
                       sys.argv[c + 1] if c < len(sys.argv) - 1 else str(v))))

the= o(**{m[1]: coerce(m[2])
          for m in re.finditer(r"-\w+\s*(\w+).*=\s*(\S+)", __doc__)}) 

if __name__ == "__main__":
  cli(the.__dict__)
  for n, s in enumerate(sys.argv):
    if fun := globals().get("eg" + s.replace("-", "_")):
      random.seed(the.rseed)
      fun(None if n == len(sys.argv) - 1 else coerce(sys.argv[n + 1]))
