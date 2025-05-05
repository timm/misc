import random, math
random.seed()

class o: 
  __init__ = lambda i,**d: i.__dict__.update(**d)
  __repr__ = lambda i: i.__class__.__name__ + str(i.__dict__)

the = o(p=2, file="data.csv", some=128)

#------------------------------------------------------------------------------
def col(txt=' ',at=0): return (Num if txt[0].isupper() else Sym)(txt,at)

class Col(o):
  def __init__(i,txt=' ',at=0): 
    i.txt, i.at, i.n, i.w, i.goal = txt, at, 0, 0, txt[-1] in "!+-"
    
  def sub(i,x,n=1): return i.add(x, n=n, flip=-1)

#------------------------------------------------------------------------------
class Num(Col):
  def __init__(i,*_):
    super().__init__(*_); 
    i.lo, i.hi, i.mu, i.m2 = 1e32, -1e32, 0, 0 
    i.goal = 0 if txt[-1]=="-" else 1
    
  def add(i, x, n=1, flip=1): 
    if x!="?": 
     i.n += flip*n
     i.lo = min(i.lo,x); i.hi=max(i.hi,x)
     if flip < 0 and i.n < 2:
       i.mu = i.sd = 0
     else:
       d = x-i.mu
       i.mu += flip*d/i.n
       i.m2 += flip*d*(x - i.mu)
       
  def dist(i,a,b):
    a,b = i.norm(a), i.norm(b)
    a = a if a!="?" else (1 if b<.5 else 0)
    b = b if b!="?" else (1 if a<.5 else 0)
    return abs(a - b)
  def norm(i,x): return x if x=="?" else (x-i.lo)/(i.hi-i.lo+1e-32)
  def mid(i): return i.mu
  def var(i): return i.n < 2 and 0 or (max(0,i.m2)/(i.n - 1))^0.5

#------------------------------------------------------------------------------
class Sym(Col):
  def __init__(i,*_): super().__init__(*_); i.has={}
  def add(i,x, n=1,flip=1): 
    if x!="?": 
      i.n += flip*n
      i.has[x] = i.has.get(x,0) + flip*n
  def norm(i,x): return x
  def dist(i,a,b): return a!=b
  def mid(i): max(i.has, key=i.has.get)
  def var(i): return -sum(n/i.n * math.log(n/i.n,2) for n in i.has.values())

#------------------------------------------------------------------------------
class Data(o):
  def __init__(i,src=[]): 
    i.rows,i.cols = [],None
    [i.add(row) for row in src]
  def clone(i, rows=[]):
    return Data([[o.cols.name] + rows])
  def add(i,row):
    if i.cols: i.rows += [i.cols.add(row)]
    else i.cols=Cols(row)
  def sub(i,row,purge=True):
    i.cols.sub(row)
    if purge: i.rows.remove(row)
  def dist(i,a, b, p=the.p):
    def fun(c): return c.w * c.dist(a[c.at], b[c.at])
    return (sum(fun(c)**p for c in i.cols.x) / len(i.cols.x))**(1/p)
  def kpp(i,k, rows=None):
    rows = rows or i.rows
    row1,*rows = random.choices(rows, k=min(len(rows,the.some))
    out = [row1]
    while len(out) < k:
      tmp = [min(i.dist(x, y)**2 for y in out) for x in rows]
      r = random.random() * sum(tmp)
      for j, x in enumerate(tmp):
        r -= x
        if r < 0:
          out += [rows[j]]
          break
    return out
  
#------------------------------------------------------------------------------
class Cols(o):
  def __init__(i,names):
    i.x, i.y, i.names,i.klass = [],[],names,None
    i.all = [col(s, j) for j, s in enumerate(names)]
    for c in i.all: 
      if c.txt[-1] != "X":
        if c.txt[-1] == "!": i.klass = c
        (i.y if c.txt in "!+-" else i.x).append(c)
  def add(i,row): return [c.add(row[c.at]) for c in i.all]
  def sub(,row) : return [c.sub(row[c.at]) for c in i.all]
  
#------------------------------------------------------------------------------
def csv(file):
  for line in open(file):
    if s := line.strip():
      yield [coerce(x) for x in s.split(",")]  

def coerce(x):
  try: return int(x) 
  except: 
    try: return float(x) 
    except: return x

def adds(src=[], out=None):
  for x in src:
    out = out or (Sym if type(x)=="string" else Num)()
    out.add(x)
  return out

#------------------------------------------------------------------------------
def eq(x,y): return x == y
def le(x,y): return x <= y
def gt(x,y): return x >  y

# too har d wirred into nums
def tree(rows,data, Y,Klass):
  bestE, out = 1e32, None
  cuts = sorted([c.cuts(rows,Y,Klass)                        
                 for c in data.cols.x], key=lambda x:x.var)
  if cuts:
    for cut in all[0]:

def values(i,rows):
  for row in rows:
    x = row[i.at]
    if x != "?": yield x,row

def Sym.cuts(i,rows,Y,Klass):
  tmp={}
  for x,row in i.values(rows):
    tmp[x] = tmp.get(x) or Klass()
    tmp[x].add(Y(row))
  return o(var = tmp[x].var(), X WRONG has to be the sum
          decisions = [o(at=i.at, txt=i.txt.x=x, decide=eq)]
                         for x in tmp)
      
def Num.cuts(i,rows,Y,Klass):
  least,out = 1E32, None,
  L, R = Klass(), adds([Y(row) for row in rows], Klass())
  for x,row in i.values(rows):
    R.sub(L.add(Y(row)))
    if x != b4:
      e = (L.n * L.var() + R.n * R.var()) / len(rows)
      if e < least:
        least, out = e, o(var = e, 
                         decisions = [o(at=i.at, txt=i.txt, x=x, decide=le),
                                      o(at=i.at, txt=i.txt, x=x, decide=gt)])
    b4 = x
  return out
  
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

