#!/usr/bin/env python3 -B
# In this code "i" == "self". Also "_x" is a hidden var (to big, or to secret, to show).
"""
abc.py : XAI for active learning + multi-objective optimization
A=a few random choices; B=build a model incrementally, C=check it on new data
(c) 2025, Tim Menzies <timm@ieee.org>, MIT License

Options:
      -A A       samples for a few random picks     = 4
      -B B       samples for active learning build  = 30
      -C C       samples for tests                  = 5
      -a acq     acq function (xploit,xplor,adapt)  = xplot
      -b bins    number of bins                     = 5
      -M Min     minPts per cluster (0=auto choose) = 0
      -P P       distance formula exponent          = 2
      -d dims    number of dimensions               = 4
      -F Few     how many to test in acquire        = 100
      -f file    training csv file = ../../moot/optimize/misc/auto93.csv
      -g guess   how to divide best/rest            = 0.5
      -k k       Bayes hack (for rare classes)      = 1
      -l leaf    min leaf size                      = 2
      -m m       Bayes hack (for rare frequencies)  = 2
      -r rseed   random number seed                 = 1234567891
      -s some    search space size for poles        = 30
"""
import random, sys, re
import math
from typing import List,Dict,Any,Union,Tuple,Optional,Callable,Iterator,TypeVar,cast

# Short cuts
BIG  = 1e32
any  = random.choice
many = random.choices

# Types
Atom = Union[int, float, str, bool]
Row  = List[Atom]
Rows = List[Row]
Col  = Union['Sym', 'Num']

def cat(x: Any) -> str:
  "Convert any object to a string representation."
  isa = isinstance
  if isa(x, list): return "{" + ", ".join(map(cat, x)) + "}"
  if isa(x, (float, int)): return str(int(x)) if x == int(x) else f"{x:.3g}"
  if isa(x, dict): return cat([f":{k} {cat(v)}" for k, v in x.items() if str(k)[0]!="_"])
  if hasattr(x, "__dict__"): return x.__class__.__name__ + cat(x.__dict__)
  return str(x)

class o:
  "Base class providing dictionary update and string representation."
  __init__    = lambda i, **d: i.__dict__.update(**d)
  __getitem__ = lambda i, k  : i.__dict__[k]
  __repr__    = cat

#----------------------------------------------------------------------------
# ## Sym

class Sym(o):
  "Symbol class for handling categorical attributes."

  def __init__(i, has = [], at: int = 0, txt: str = " "):
    "INIT: Initialize a symbol column."
    i.at: int = at            # Column position
    i.txt: str = txt          # Column name
    i.n: int = 0              # Count of items seen
    i.has: Dict[Atom, int] = {}  # Frequency counts of values
    [i.add(x) for x in has]

  def add(i, x: Atom, inc: bool = True) -> Atom:
    "ADD: Add a value to the symbol column."
    if x != "?":
      step = 1 if inc else -1
      i.n += step
      i.has[x] = step + (i.has[x] if x in i.has else 0)
    return x

  def like(i, x:str, prior:float):
    "BAYES: return likelihood `x` belongs to self."
    return (i.has.get(x,0) + the.m*prior) / (i.n + the.m + 1/BIG)

  def dist(i, x: Atom, y: Atom) -> float:
    "DIST: Calculate distance between two symbols."
    return x == "?" and y == "?" and 1 or x != y

  def mid(i) -> Atom:
    "STATS: Get the most common value."
    return max(i.has, key=i.has.get)

  def div(i) -> float:
    "STATS: Calculate entropy as diversity measure."
    return -sum(v / i.n * math.log(v / i.n, 2) for v in i.has.values() if v > 0)

  def cuts(i,rows,Y,Klass) -> o:
    "TREE: report results of splitting rows on this column."
    n,d = 0,{}
    for row in rows:
      x = row[i.at] 
      if x != "?":
        n = n + 1
        d[x] = d.get(x) or Klass()
        d[x].add(Y(row))
    return o(div = sum(c.n/n * c.div() for c   in d.values()),
             decisions= [("==",c.at,k) for k,v in d.items()])

#----------------------------------------------------------------------------
# ## Num

class Num(o):
  "Number class for handling numeric attributes."

  def __init__(i, has: List[Atom] = [], at: int = 0, txt: str = " "):
    "INIT: Initialize a numeric column."
    i.at: int = at            # Column position
    i.txt: str = txt          # Column name
    i.n: int = 0              # Count of items seen
    i.mu: float = 0           # Mean of values
    i.m2: float = 0           # Sum of squared differences from mean
    i.lo: float = BIG         # Lowest value seen
    i.hi: float = -BIG        # Highest value seen
    i.heaven: int = 0 if txt[-1] == "-" else 1  # Optimization goal (0=min, 1=max)
    [i.add(x) for x in has]

  def add(i, x: Atom, inc: bool = True) -> Atom:
    "ADD: Add a value to the numeric column."
    if x != "?":
      i.lo = min(x, i.lo)
      i.hi = max(x, i.hi)
      if not inc and i.n <= 2:
        i.n = i.mu = i.m2 = 0
      else:
        step = 1 if inc else -1
        i.n += step
        d = x - i.mu
        i.mu += step * d / i.n
        i.m2 += step * d * (x - i.mu)
    return x

  def like(i, x:(int|float), _ ):
    "BAYES: return likelihood `x` belongs to self."
    sd = i.div() or 1 / BIG
    var = 2 * sd * sd
    z = (x - i.mu) ** 2 / var
    return min(1, max(0, math.exp(-z) / (math.tau * var) ** 0.5))
    
  def dist(i, x: Atom, y: Atom) -> float:
    "DIST: Calculate distance between two numeric values."
    if x == "?" and y == "?": return 1
    x = i.norm(x) if x != "?" else (0 if y > 0.5 else 1)
    y = i.norm(y) if y != "?" else (0 if x > 0.5 else 1)
    return abs(x - y)

  def div(i) -> float:
    "STATS: Calculate standard deviation as diversity measure."
    return 0 if i.n <= 2 else (max(0, i.m2) / (i.n - 1))**0.5

  def mid(i) -> float:
    "STATS: Get the mean value."
    return i.mu

  def norm(i, x: Atom) -> float:
    "STATS: Normalize value to range 0-1."
    return (float(x) - i.lo) / (i.hi - i.lo + 1 / BIG)

def cuts(i, rows, Y, Klass) -> o:
  "TREE: report results of splitting rows on this column."
  out, b4, lhs, rhs = None, None, Klass(), Klass()
  xys = [(r[i.at], rhs.add( i.ydist(r))) for r in rows if r[i.at] != "?"]
  xpect = rhs.div()
  for x, y in sorted(xys, key=lambda xy: x[0]):
    if x != b4:
      if the.leaf <= lhs.n <= len(xys) - the.leaf:
        tmp = (lhs.n * lhs.div() + rhs.n * rhs.div()) / len(xys)
        if tmp < xpect:
          xpect, out = tmp, [("<=", i.at, b4), (">", i.at, b4)]
    lhs.add( rhs.add(y, inc=False))
    b4 = x
  if out:
    return o(div=xpect, decisions=out)

#----------------------------------------------------------------------------
# ## Data

class Data(o):
  "Data class for handling collections of rows."

  def __init__(i, src: Iterator[Row]):
    "INIT: Initialize from data source."
    i._rows: Rows = []             # Storage for data rows
    i.cols = o(x=[], y=[], all=[]) # Track columns (x=independent, y=dependent, all=all)
    src = iter(src)
    [i.about(c, s) for c, s in enumerate(next(src))]
    [i.add(row) for row in src]

  def about(i, c: int, s: str) -> None:
    "INIT: Set up column information."
    col: Col = (Num if s[0].isupper() else Sym)(at=c, txt=s)
    i.cols.all += [col]
    if s[-1] != "X":
      (i.cols.y if s[-1] in "-+" else i.cols.x).append(col)

  def clone(i, rows: Rows = []) -> 'Data':
    "INIT: Create a new data with same structure but different rows."
    return Data([[col.txt for col in i.cols.all]] + rows)

  def minPts(i) -> int:
    "INIT: Report how many points are needed for each bucket."
    out = the.Min
    if out==0:
      out = 2 if len(i._rows) <  30 else (
            3 if len(i._rows) < 100 else 2 + the.dims)
    return out
    
  def add(i, row: Row, inc: bool = True, purge: bool = False) -> Row:
    "ADD: Add a row to the data."
    if purge: i._rows.remove(row)  # can be slow. disabled by default
    elif inc: i._rows += [row]
    for col in i.cols.all: col.add(row[col.at], inc)
    return row

  def guesses(i):
    "BAYES: Split rows to best,rest. Label row that's e.g. max best/rest."
    def _acq(b, r, acq="xploit", p=1):
      b,r = math.e**b, math.e**r
      q = 0 if acq=="xploit" else (1 if acq=="xplor" else 1-p)
      return (b + r*q) / abs(b*q - r + 1/BIG)
    def _guess(row):
      return _acq(best.like(row,n,2), rest.like(row,n,2), the.acq, n/the.B)

    random.shuffle(i._rows)
    n         = the.A
    todo      = i._rows[n:]
    bestrest  = i.clone( i._rows[:n] )
    done      = bestrest.ysort()
    cut       = round(n**the.guess)
    best,rest = i.clone(done[:cut]), i.clone(done[cut:])
    while len(todo) > 2 and n < the.B:
      n      += 1
      hi, *lo = sorted(todo[:the.Few*2], key=_guess, reverse=True)
      todo    = lo[:the.Few] + todo[the.Few*2:] + lo[the.Few:]
      bestrest.add( best.add(hi))
      best._rows = bestrest.ysort(best._rows)
      if len(best._rows) >= round(n**the.guess):
         rest.add( best.add( best._rows.pop(-1), inc=False))
    return o(best=best, rest=rest, test=todo)
  
  def like(i,row:Row, nall:int, nh:int) -> float:
    "BAYES: Return how much this data likes `row`."
    prior = (len(i._rows) + the.k) / (nall + the.k*nh)
    tmp = [col.like(row[col.at],prior) for col in i.cols.x if row[col.at] != "?"]
    return sum(math.log(n) for n in tmp + [prior] if n>0)

  def clusters(i, poles: Rows) -> Dict[Tuple[int, ...], 'Data']:
    "CLUSTER: Locality sensitive hashing to cluster rows by projection."
    clusters: Dict[Tuple[int, ...], 'Data'] = {}
    for row in i._rows:
      k = tuple(i.project(row, a, b) for a, b in zip(poles, poles[1:]))
      clusters[k] = clusters.get(k) or i.clone()
      clusters[k].add(row)
    return clusters

  def poles(i) -> Rows:
    "CLUSTER: Select poles at max distance to poles picked so far."
    r0, *some = many(i._rows, k=the.some + 1)
    out = [max(some, key=lambda r1: i.xdist(r1, r0))]
    for _ in range(the.dims):
      out += [max(some, key=lambda r2: sum(i.xdist(r2, r1) for r1 in out))]
    return out

  def project(i, row: Row, a: Row, b: Row) -> int:
    "CLUSTER: Project a row onto the line connecting two poles, a and b"
    c = i.xdist(a, b)
    x = (i.xdist(row, a)**2 + c**2 - i.xdist(row, b)**2) / (2 * c)
    return min(int(x / c * the.bins), the.bins - 1) # return 0..the.bins-1

  def xdist(i, row1: Row, row2: Row) -> float:
    "DIST: Calculate distance between rows using only X columns."
    return dist(c.dist(row1[c.at], row2[c.at]) for c in i.cols.x)

  def ydist(i, row: Row) -> float:
    "DIST: Calculate the distance to heaven for this row."
    return dist(abs(c.norm(row[c.at]) - c.heaven) for c in i.cols.y)

  def ydists(i,rows:Rows=None) -> Num:
    "DIST: Get numeric stats on y-distances for rows."
    return Num(i.ydist(row) for row in rows or i._rows)

  def ysort(i, rows=None) -> Rows:
    "DIST: Return rows sorted by distance to heaven."
    return sorted(rows or i._rows, key=lambda row: i.ydist(row))

  def mid(i) -> Row:
    "STATS: Find the central tendency row."
    middle = [col.mid() for col in i.cols.all]
    return min(i._rows, key=lambda row: i.xdist(row, middle))
    
  def tree(i, rows, Klass=Num, decision=None):
    "TREE: grow a tree from here."
    t = i.clone(rows)
    t.kids = []
    t.decision = decision
    t.ys = i.ydists(rows)
    if len(rows) >= the.leaf:
      cuts = [tmp for c in t.cols.x if (tmp := c.cuts(rows,i.dist,Klass=Klass))]    
      if cuts:
        for decision in sorted(cuts, key=lambda cut: cut.div)[0].decisions:
          rows1 = [row for row in rows if selects(row, *decision)]
          if the.leaf <= len(rows1) < len(rows):
            t.kids += [i.tree(rows1, Klass=Klass, decision=decision)]  
    return t

  def nodes(i, lvl=0, key=None):
    "TREE: interate over the tree."
    yield lvl,i
    for kid in (sorted(i.kids, key=key) if key else i.kids):
      for j in kid.nodes(lvl+1, key=key):
        yield j
                  
#----------------------------------------------------------------------------
# ## Functions

ops = {'<=' : lambda x,y: x <= y,
       "==" : lambda x,y: x == y,
       '>'  : lambda x,y: x >  y}

def selects(row, op, at, y):
  "LIB: return true if `(op,at,y)` selects for row."
  x = row[op]
  return  x=="?" or ops[op](x,y) 

def likes(datas, row):
  "BAYES: Return the `data` that most `likes` the `row."
  n = sum(len(data._rows) for data in datas)
  return max(datas, key=lambda data: data.like(row, n, len(datas)))

def cli(d: Dict[str, Any]) -> None:
  "LIB: Process command line arguments."
  for k, v in d.items():
    for c, arg in enumerate(sys.argv):
      if arg == "-" + k[0]:
        d[k] = coerce("False" if str(v) == "True" else (
                      "True" if str(v) == "False" else (
                       sys.argv[c + 1] if c < len(sys.argv) - 1 else str(v))))

def coerce(x: str) -> Atom:
  "LIB: Convert string to appropriate type."
  for what in (int, float):
    try: return what(x)
    except: pass
  x = x.strip()
  y = x.lower()
  return (y == "true") if y in ("true", "false") else x

def csv(path: str) -> Iterator[Row]:
  "LIB: Read csv file into a generator of rows."
  with open(path) as f:
    for line in f:
      yield [coerce(x) for x in line.strip().split(",")]

def dist(dims: Iterator[float]) -> float:
  "LIB: Calculate Minkowski distance."
  total, n = 0, 1 / BIG
  for x in dims:
    n += 1
    total += x**the.P
  return (total / n)**(1 / the.P)
                  
#----------------------------------------------------------------------------
# ## Examples

def eg_h(_) -> None:
  "Print help text."
  print(__doc__,"\nExamples:")
  for s,fun in globals().items():
    if s.startswith("eg__"): 
      print(f"  {re.sub('eg__','--',s):>11}    {fun.__doc__}")

def eg__all(_) -> None:
  "Run all examples."
  for f in [eg__the, eg__csv, eg__data, eg__ydist, eg__poles, eg__counts]:
    random.seed(the.rseed)
    print(f"\n\n{'-'*80}\n\n## {f.__name__}\n\n# {f.__doc__}\n")
    f(_)

def eg__about(_): 
  import pydoc, sys; print(pydoc.render_doc(sys.modules[__name__]))

def eg__the(_) -> None: 
  "Print the configuration."
  print(the)

def eg__csv(_) -> None:
  "Print csv data."
  [print(row) for row in csv(the.file)]

def eg__sym(_) -> None:
  "Illustrates `Sym` column operations: creation, mode, and entropy."
  sym = Sym("aaaabbc")       
  print("OLD:", o(mode=sym.mid(), sd=sym.div()))
  [sym.add(x) for x in "dde"]
  print("NEW:", o(mode=sym.mid(), sd=sym.div()))  
  assert 2.12 < sym.div() < 2.13 and sym.mid() == "a" 

def eg__num(_) -> None:
  "Illustrates `Num` column operations: creation, normalization, mean, std dev."
  mu, sd, n = 10, 1, 10**3
  num = Num(random.gauss(mu,sd) for _ in range(n))
  print("OLD:",o(mu=num.mid(), sd=num.div(), like=num.like(10,1)))
  [num.add(random.gauss(mu,sd))  for _ in range(n)]
  print("New:",o(mu=num.mid(), sd=num.div()))
  assert (10 < num.mid() < 10.03) and (1 < num.div() < 1.03)
 
def eg__data(_) -> None:
  "Print column information."
  d = Data(csv(the.file))
  [print("x", col) for col in d.cols.x]
  [print("y", col) for col in d.cols.y]
  assert len(d._rows)==398 and d.cols.x[0].lo==3 and d.cols.y[0].heaven==0

def eg__data(_) -> None:
  "Print column information."
  d = Data(csv(the.file))
  [print("x", col) for col in d.cols.x]
  [print("y", col) for col in d.cols.y]
  assert len(d._rows)==398 and d.cols.x[0].lo==3 and d.cols.y[0].heaven==0

def eg__incFalse(_) -> None:
  "Show that rows can be incrementally added and deleted to a Data." 
  d   = Data(csv(the.file))
  d1  = d.clone()
  two = lambda c: (round(c.mid(),3), round(c.div(),3))
  for row in d._rows: 
    d1.add(row)
    if len(d1._rows) == 100: a1,a2 = two(d1.cols.y[0]), two(d1.cols.x[3])
  for row in d._rows[::-1]: 
    d1.add(row,inc=False,purge=True)
    if len(d1._rows) == 100: 
      assert (a1,a2) == (two(d1.cols.y[0]), two(d1.cols.x[3]))

def eg__likes(_) -> None:
  d = Data(csv(the.file))
  num = Num(d.like(row, 1000, 2) for row in d._rows)
  assert -18 < num.lo and num.hi < -11
  
def eg__ydist(_) -> None:
  "Print rows sorted by distance to heaven."
  def gap(row): 
      out = d.ydist(row)
      assert 0 <= out <= 1
      return out      
  d    = Data(csv(the.file))
  rows = sorted(d._rows, key=gap)
  for row in rows[:3]: print("good", row)
  for row in rows[-3:]: print("bad", row)
  assert abs(gap(rows[0]) - gap(rows[-1])) > 0.5

def eg__guesses(file=None) -> None:
  d = Data(csv(file or the.file))
  for _ in range(20):
    out =  d.guesses() 
    def guess(row):
        return out.best.like(row,the.B,2) - out.rest.like(row,the.B,2)
    some= sorted(out.test, key=guess, reverse=True)[:the.C]
    print(o(b4    = d.ydists().mu,
            lo    = d.ydists().lo,
            best  = d.ydists(out.best._rows).mu, 
            rest  = d.ydists(out.rest._rows).mu, 
            test  = d.ydists(some).mu))

def eg__xdist(_) -> None:
  "Illustrate `dist` operations"
  def gap(row2): 
      out = d.xdist(row1,row2)
      assert 0 <= out <= 1
      return out
  d    = Data(csv(the.file))
  row1 = d._rows[0]
  rows = sorted(d._rows, key=gap)
  for row2 in rows[:3] + rows[-3:]: print(o(row=row2, gap=gap(row2)))
  col = d.cols.x[0]
  assert 1 == abs(col.norm(rows[0][col.at]) - col.norm(rows[-1][col.at]))
  
def eg__poles(_) -> None:
  "Show clustering dimensions."
  d = Data(csv(the.file))
  poles = d.poles()
  for n,(row1,row2) in enumerate(zip(poles, poles[1:])):
    print(o(dim=n, length=d.xdist(row1,row2), pole=row1))

def eg__centroids(_) -> None:
  "Show clustering dimensions."
  d = Data(csv(the.file))
  for centroid in d.clusters(d.poles()): print(centroid)

def eg__counts(file: str = None) -> None:
  "Show cluster counts and stats."
  d = Data(csv(file or the.file))
  enough = d.minPts()
  twos = [ (d1.ydists(),pos)  for pos,d1 in d.clusters(d.poles()).items()]
  for num,pos in sorted(twos, key=lambda two:two[0].mid()):
    if num.n >= enough:
      print(o(pos=pos, n=num.n, mid=num.mid()))

#----------------------------------------------------------------------------
# ## Start-up

the= o(**{m[1]: coerce(m[2])
          for m in re.finditer(r"-\w+\s*(\w+).*=\s*(\S+)", __doc__)}) 

if __name__ == "__main__":
  cli(the.__dict__)
  for n, s in enumerate(sys.argv):
    if fun := globals().get("eg" + s.replace("-", "_")):
      random.seed(the.rseed)
      fun(None if n == len(sys.argv) - 1 else coerce(sys.argv[n + 1]))
