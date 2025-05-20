class o:
  __init__    = lambda i, **d: i.__dict__.update(**d)
  __getitem__ = lambda i, k  : i.__dict__[k]
  __repr__    = lambda i: i.__class__.__name__.o(i.__dict__)

the = o(p=2)

#------------------------------------------------------------------------------
def Data(src=[]):
  src = iter(src)
  return adds(src, o(it=Data, cols=Cols(next(src)),rows=[]))

def Cols(names)
  i = o(it=Cols, all=[], x=[], y=[])
  for c,s in enumerate(names):
    i.all += [(Num if s[0].isupper() else Sym)(txt=s,at=c)]
    if s[-1] != "X":
      (i.y if s[-1] in "-" else i.x).append(i.all[-1])
  return i

def Num(lst=[], txt=" ",at=0):
  return adds(lst,o(it=Num, n=0, at=at, txt=txt, 
                    heaven=(0 if txt[-1]=="-" else 1),
                    mu=0, m2=0, lo=-BIG, hi=BIG))

def Sym(lst=[], txt=" ",at=0): 
  return adds(lst, o(it=Num, n=0, at=at, txt=txt, has={}))

#------------------------------------------------------------------------------
def add(v,i,flip=1,purge=False):
  if v != "?": 
    i.n += flip
    f = _addNum if i.it is Num else (_addSym if i.it is Sym else _addData)
    f(v,i,flip,purge)
  return v

def _addSym(v,i,flip,_):
  i.has[v] = flip + i.has.get(v,0)

def _addData(v,i,flip,purge)
  if flip < 0:  
    if purge: i.rows.remove(v)
    [add(v[col.at], col, -1) for col in i.cols.all]  
  else: 
    i.rows += [[add(v[col.at], col) for col in i.cols.all]]

def _addNum(v,i,flip,_):
  i.lo  = min(v, i.lo)
  i.hi  = max(v, i.hi)
  if flip < 0 and i.n < 2: 
    i.m2 = i.mu = i.n = 0
  else:
    d     = v - i.mu
    i.mu += flip * (d / i.n)
    i.m2 += flip * (d * (v -   i.mu))

#------------------------------------------------------------------------------
def dist(dims):
  total, n = 0, 1 / BIG
  for x in dims:
    n += 1
    total += x**the.P
  return (total / n)**(1 / the.P)

def xdist(i, row1: Row, row2: Row) -> float:
  return dist(c.dist(row1[c.at], row2[c.at]) for c in i.cols.x)

def clusters(i, poles: Rows) -> Dict[Tuple[int, ...], 'Data']:
  clusters: Dict[Tuple[int, ...], 'Data'] = {}
  for row in i._rows:
    k = tuple(i.project(row, a, b) for a, b in zip(poles, poles[1:]))
    clusters[k] = clusters.get(k) or i.clone()
    clusters[k].add(row)
  return clusters

def poles(i) -> Rows:
  r0, *some = many(i._rows, k=the.some + 1)
  out = [max(some, key=lambda r1: i.xdist(r1, r0))]
  for _ in range(the.dims):
    out += [max(some, key=lambda r2: sum(i.xdist(r2, r1) for r1 in out))]
  return out

def project(i, row: Row, a: Row, b: Row) -> int:
  c = i.xdist(a, b)
  x = (i.xdist(row, a)**2 + c**2 - i.xdist(row, b)**2) / (2 * c)
  return min(int(x / c * the.bins), the.bins - 1) # return 0..the.bins-1

#------------------------------------------------------------------------------
def at(col,row): return row.cells[col.at]

def adds(lst,i): [add(v,i) for v in lst]; return i

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
