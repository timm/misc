import random, math
random.seed()

class o: 
  __init__ = lambda i,**d: i.__dict__.update(**d)
  __repr__ = lambda i: i.__class__.__name__ + str(i.__dict__)

def col(txt=' ',at=0): return (Num if txt[0].isupper() else Sym)(txt,at)

class Col(o):
  def __init__(i,txt=' ',at=0): 
    i.txt, i.at, i.n, i.w, i.goal = txt, at, 0, 0, txt[-1] in "!+-"

class Num(Col):
  def __init__(i,*_):
    super().__init__(*_); 
    i.lo, i.hi, i.mu, i.m2 = 1e32, -1e32, 0, 0 
    i.goal = 0 if txt[-1]=="-" else 1
  def add(i,x): 
    if x!="?": 
     i.n+=1
     d = x-i.mu
     i.mu += d/i.n; i.m2 += d*(x - i.mu)
     i.lo = min(i.lo,x); i.hi=max(i.hi,x)
  def norm(i,x): 
    return x if x=="?" else (x-i.lo)/(i.hi-i.lo+1e-32)
  def dist(i,a,b):
    a,b = i.norm(a), i.norm(b)
    a = a if a!="?" else (1 if b<.5 else 0)
    b = b if b!="?" else (1 if a<.5 else 0)
    return abs(a - b)
  def mid(i): return i.mu
  def var(i): return i.n < 2 and 0 or (max(0,i.m2)/(i.n - 1))^0.5

class Sym(Col):
  def __init__(i,*_): super().__init__(*_); i.has={}
  def add(i,x): 
    if x!="?": 
      i.n+=1; i.has[x]=i.has.get(x,0)+1
  def norm(i,x): return x
  def dist(i,a,b): return a!=b
  def mid(i): max(i.has, key=i.has.get)
  def var(i): return -sum(n/i.n * math.log(n/i.n,2) for n in i.has.values())

class Data(o):
  def __init__(i,src=[]): 
    i.rows,i.cols = [],None
    [i.add(row) for row in src]
  def add(i,row):
    if i.cols: i.rows += [i.cols.add(row)]
    else: i.cols=Cols(row)
  def dist(i,a, b, p=the.p):
    def fun(c): return c.w * c.dist(a[c.at], b[c.at])
    return (sum(fun(c)**p for c in i.cols.x) / len(i.cols.x))**(1/p)
  def kpp(i,k, rows=None):
    row1,*rows = rows or shuffle(i.rows)[:128]
    out = [row1]
    while len(out) < k:
      tmp = [min(i.dist(x, y)**2 for y in out) for x in rows]
      r = random.random() * sum(tmp)
      for i, x in enumerate(tmp):
        r -= x
        if r < 0:
          out += [rows[i]]
          break
    return out
  
class Cols(o):
  def __init__(i,names):
    i.x, i.y, i.names,i.klass = [],[],names,None
    i.all = [col(s, j) for j, s in enumerate(names)]
    for c in i.all: 
      if c.txt[-1] != "X":
        (y if c.txt in "!+-" else x).append(c)
      if c.txt[-1] == "!": i.klass=col
  def add(i,row):
    for c in i.all: c.add(row[c.at])
    return row

def csv(file):
  for line in open(file):
    if s := line.strip():
      yield [coerce(x) for x in s.split(",")]  

def coerce(x):
  try: return int(x) 
  except: 
    try: return float(x) 
    except: return x

the = o(p=2)

def any(a): return random.choice(a)

def ent(rows):
  f = {}
  for x in rows: f[x[-1]] = f.get(x[-1], 0) + 1
  n = len(rows)
  return -sum((v / n) * math.log2(v / n) for v in f.values())

def tree(rows):
  bestE, bestJ, bestV = 1e9, None, None
  m = len(rows[0]) - 1
  for j in range(m):
    for v in {x[j] for x in rows}:
      L = [x for x in rows if x[j] <= v]
      R = [x for x in rows if x[j] > v]
      if L and R:
        e = (len(L) * ent(L) + len(R) * ent(R)) / len(rows)
        if e < bestE:
          bestE, bestJ, bestV = e, j, v
  if bestJ is None: return set()
  L = [x for x in rows if x[bestJ] <= bestV]
  return {bestJ} | tree(L)

def select(data, cols, k=16, g=5):
  m = len(data[0])
  w = [1] * m
  for _ in range(g):
    centers = kpp(k, data, cols)
    rows = []
    for x in data:
      i = min(range(k), key=lambda j: dist(x, centers[j], cols))
      rows.append(x + [chr(97 + i)])
    keep = tree(rows)
    w = [w[i] if i in keep else 0 for i in range(m)]
  return [i for i, v in enumerate(w) if v > 0]

