#!/usr/bin/env python3 -B
# In this code "i" == "self". Also "_x" is a hidden var (to big, or to secret, to show).
"""
kube.py : barelogic, XAI for active learning + multi-objective optimization
(c) 2025, Tim Menzies <timm@ieee.org>, MIT License

Options:
      -b bins    number of bins                     = 5
      -m min     minPts per cluster (0=auto choose) = 0
      -P P       distance formula exponent          = 2
      -d dims    number of dimensions               = 4
      -r rseed   random number seed                 = 1234567891
      -s some    search space size for poles        = 30
      -f file    training csv file = ../../moot/optimize/misc/auto93.csv
"""
import random, sys, re
import math
from typing import List,Dict,Any,Union,Tuple,Optional,Callable,Iterator,TypeVar,cast

any = random.choice
many = random.choices

BIG = 1e32

# Type aliases
Atom = Union[int, float, str, bool]
Row  = List[Atom]
Rows = List[Row]
Col  = Union['Sym', 'Num']

def cat(x: Any) -> str:
  """Convert any object to a string representation."""
  isa = isinstance
  if isa(x, list): return "{" + ", ".join(map(cat, x)) + "}"
  if isa(x, (float, int)): return str(int(x)) if x == int(x) else f"{x:.3g}"
  if isa(x, dict): return cat([f":{k} {cat(v)}" for k, v in x.items() if str(k)[0]!="_"])
  if hasattr(x, "__dict__"): return x.__class__.__name__ + cat(x.__dict__)
  return str(x)

class o:
  """Base class providing dictionary update and string representation."""
  __init__ = lambda i, **d: i.__dict__.update(**d)
  __repr__ = cat

# ----------------------------------------------------------------------------------------
class Sym(o):
  """Symbol class for handling categorical attributes."""

  def __init__(i, has: List[Atom] = [], at: int = 0, txt: str = " "):
    """Initialize a symbol column."""
    i.at: int = at            # Column position
    i.txt: str = txt          # Column name
    i.n: int = 0              # Count of items seen
    i.has: Dict[Atom, int] = {}  # Frequency counts of values
    [i.add(x) for x in has]

  def add(i, x: Atom, inc: bool = True) -> Atom:
    """Add a value to the symbol column."""
    if x != "?":
      step = 1 if inc else -1
      i.n += step
      i.has[x] = step + (i.has[x] if x in i.has else 0)
    return x

  def dist(i, x: Atom, y: Atom) -> float:
    """Calculate distance between two symbols."""
    return x == "?" and y == "?" and 1 or x != y

  def mid(i) -> Atom:
    """Get the most common value."""
    return max(i.has, key=i.has.get)

  def div(i) -> float:
    """Calculate entropy as diversity measure."""
    return -sum(v / i.n * math.log(v / i.n, 2) for v in i.has.values() if v > 0)

# ----------------------------------------------------------------------------------------
class Num(o):
  """Number class for handling numeric attributes."""
  def __init__(i, has: List[Atom] = [], at: int = 0, txt: str = " "):
    """Initialize a numeric column."""
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
    """Add a value to the numeric column."""
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

  def dist(i, x: Atom, y: Atom) -> float:
    """Calculate distance between two numeric values."""
    if x == "?" and y == "?": return 1
    x = i.norm(x) if x != "?" else (0 if y > 0.5 else 1)
    y = i.norm(y) if y != "?" else (0 if x > 0.5 else 1)
    return abs(x - y)

  def mid(i) -> float:
    """Get the mean value."""
    return i.mu

  def norm(i, x: Atom) -> float:
    """Normalize value to range 0-1."""
    return (float(x) - i.lo) / (i.hi - i.lo + 1 / BIG)

  def div(i) -> float:
    """Calculate standard deviation as diversity measure."""
    return 0 if i.n <= 2 else (max(0, i.m2) / (i.n - 1))**0.5

# ----------------------------------------------------------------------------------------
class Data(o):
  """Data class for handling collections of rows."""
  def __init__(i, src: Iterator[Row]):
    """Initialize from data source."""
    i._rows: Rows = []             # Storage for data rows
    i.cols = o(x=[], y=[], all=[]) # Track columns (x=independent, y=dependent, all=all)
    src = iter(src)
    [i.about(c, s) for c, s in enumerate(next(src))]
    [i.add(row) for row in src]

  def about(i, c: int, s: str) -> None:
    """Set up column information."""
    col: Col = (Num if s[0].isupper() else Sym)(at=c, txt=s)
    i.cols.all += [col]
    if s[-1] != "X":
      (i.cols.y if s[-1] in "-+" else i.cols.x).append(col)

  def add(i, row: Row, inc: bool = True, purge: bool = False) -> Row:
    """Add a row to the data."""
    if purge: i._rows.remove(row)  # can be slow. disabled by default
    elif inc: i._rows += [row]
    for col in i.cols.all: col.add(row[col.at], inc)
    return row

  def clone(i, rows: Rows = []) -> 'Data':
    """Create a new data with same structure but different rows."""
    return Data([[col.txt for col in i.cols.all]] + rows)

  def lsh(i, poles: Rows) -> Dict[Tuple[int, ...], 'Data']:
    """Locality sensitive hashing to group rows by projection."""
    clusters: Dict[Tuple[int, ...], 'Data'] = {}
    for row in i._rows:
      k = tuple(i.project(row, a, b) for a, b in zip(poles, poles[1:]))
      clusters[k] = clusters.get(k) or i.clone()
      clusters[k].add(row)
    return clusters

  def mid(i) -> Row:
    """Find the central tendency row."""
    middle = [col.mid() for col in i.cols.all]
    return min(i._rows, key=lambda row: i.xdist(row, middle))

  def minPts(i) -> int:
    """Report how many points are needed for each bucket."""
    out = the.min
    if out==0:
      if   len(i._rows) <  30: out= 2
      elif len(i._rows) < 100: out= 3
      else: out = 2 + the.dims
    return out

  def poles(i) -> Rows:
    """Select poles at max distance to poles picked so far."""
    r0, *some = many(i._rows, k=the.some + 1)
    out = [max(some, key=lambda r1: i.xdist(r1, r0))]
    for _ in range(the.dims):
      out += [max(some, key=lambda r2: sum(i.xdist(r2, r1) for r1 in out))]
    return out

  def project(i, row: Row, a: Row, b: Row) -> int:
    """Project a row onto the line connecting two poles, a and b"""
    c = i.xdist(a, b)
    x = (i.xdist(row, a)**2 + c**2 - i.xdist(row, b)**2) / (2 * c)
    return min(int(x / c * the.bins), the.bins - 1) # return 0..the.bins-1

  def xdist(i, row1: Row, row2: Row) -> float:
    """Calculate distance between rows using only X columns."""
    return dist(c.dist(row1[c.at], row2[c.at]) for c in i.cols.x)

  def ydist(i, row: Row) -> float:
    """Calculate the distance to heaven for this row."""
    return dist(abs(c.norm(row[c.at]) - c.heaven) for c in i.cols.y)

  def ydists(i) -> Num:
    """Get numeric stats on y-distances for rows."""
    return Num(i.ydist(row) for row in i._rows)

# ----------------------------------------------------------------------------------------
def cli(d: Dict[str, Any]) -> None:
  """Process command line arguments."""
  for k, v in d.items():
    for c, arg in enumerate(sys.argv):
      if arg == "-" + k[0]:
        d[k] = coerce("False" if str(v) == "True" else (
                      "True" if str(v) == "False" else (
                       sys.argv[c + 1] if c < len(sys.argv) - 1 else str(v))))

def coerce(x: str) -> Atom:
  """Convert string to appropriate type."""
  for what in (int, float):
    try: return what(x)
    except: pass
  x = x.strip()
  y = x.lower()
  return (y == "true") if y in ("true", "false") else x

def csv(path: str) -> Iterator[Row]:
  """Read csv file into a generator of rows."""
  with open(path) as f:
    for line in f:
      yield [coerce(x) for x in line.strip().split(",")]

def dist(dims: Iterator[float]) -> float:
  """Calculate Minkowski distance."""
  total, n = 0, 1 / BIG
  for x in dims:
    n += 1
    total += x**the.P
  return (total / n)**(1 / the.P)

# ---------------------------------------------------------------------------------------/
def eg_h(_: Any) -> None:
  """Print help text."""
  print(__doc__,"\nExamples:")
  for s,fun in globals().items():
    if s.startswith("eg__"): 
      print(f"  {re.sub('eg__','--',s):>11}    {fun.__doc__}")

def eg__all(_: Any) -> None:
  """Run all examples."""
  for f in [eg__the, eg__csv, eg__data, eg__ydist, eg__poles, eg__counts]:
    random.seed(the.rseed)
    f(_)

def eg__the(_: Any) -> None: 
  """Print the configuration."""
  print(the)

def eg__csv(_: Any) -> None:
  """Print csv data."""
  [print(row) for row in csv(the.file)]

def eg__data(_: Any) -> None:
  """Print column information."""
  d = Data(csv(the.file))
  [print("x", col) for col in d.cols.x]
  [print("y", col) for col in d.cols.y]

def eg__ydist(_: Any) -> None:
  """Print rows sorted by distance to heaven."""
  d = Data(csv(the.file))
  lst = sorted(d._rows, key=lambda row: d.ydist(row))
  for row in lst[:4]: print("good", row)
  for row in lst[-4:]: print("bad", row)

def eg__poles(file: str = None) -> None:
  """Show clustering dimensions."""
  d = Data(csv(file or the.file))
  p = d.poles()
  dims = d.lsh(p)
  [print(k) for k in dims]
  print(len(dims))

def eg__counts(file: str = None) -> None:
  """Show cluster counts and stats."""
  d = Data(csv(file or the.file))
  clusters = d.lsh(d.poles())
  for data in clusters.values():
    ys = data.ydists()
    if len(data._rows) >= d.minPts():
      print(o(mid=ys.mid(), div=ys.div(), n=ys.n))


# ---------------------------------------------------------------------------------------
the = o(**{m[1]: coerce(m[2])
           for m in re.finditer(r"-\w+\s*(\w+).*=\s*(\S+)", __doc__)}) # Parse docstring

if __name__ == "__main__":
  cli(the.__dict__)
  for n, s in enumerate(sys.argv):
    if fun := globals().get("eg" + s.replace("-", "_")):
      random.seed(the.rseed)
      fun(None if n == len(sys.argv) - 1 else coerce(sys.argv[n + 1]))
