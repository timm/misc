import random, math
random.seed() 

class o: 
  __init__ = lambda i,**d: i.__dict__.update(**d)
  __repr__ = lambda i: i.__class__.__name__ + str(i.__dict__)

the = o(p=2, file="data.csv", some=128, leaf=2)

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
    i.goal = 0 if i.txt[-1]=="-" else 1

  def mid(i)   : return i.mu
  def norm(i,x): return x if x=="?" else (x-i.lo)/(i.hi-i.lo+1e-32)
  def var(i)   : return i.n < 2 and 0 or (max(0,i.m2)/(i.n - 1))^0.5
    
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

  def cuts(i, rows, Y, Klass):
    least,out = 1E32, None,
    L, R = Klass(), adds([Y(row) for row in rows], Klass())
    for x,row in i.values(rows):
      R.sub(L.add(Y(row)))
      if x != b4:
        e = (L.n * L.var() + R.n * R.var()) / len(rows)
        if e < least:
          least, out = e, o(var = e, 
                            tests = [o(at=i.at, txt=i.txt, x=x, test=le),
                                     o(at=i.at, txt=i.txt, x=x, tes=gt)])
      b4 = x
    return out
     
  def dist(i,a,b):
    a,b = i.norm(a), i.norm(b)
    a = a if a!="?" else (1 if b<.5 else 0)
    b = b if b!="?" else (1 if a<.5 else 0)
    return abs(a - b)
 
#------------------------------------------------------------------------------
class Sym(Col):
  def __init__(i,*_): super().__init__(*_); i.has={}

  def dist(i,a,b): return a!=b
  def mid(i)     : max(i.has, key=i.has.get)
  def norm(i,x)  : return x
  def var(i)     : return -sum(n/i.n*math.log(n/i.n,2) for n in i.has.values())

  def add(i,x, n=1,flip=1): 
    if x!="?": 
      i.n += flip*n
      i.has[x] = i.has.get(x,0) + flip*n

  def cuts(i, rows, Y, Klass):
    n,tmp = 0,{}
    for x,row in i.values(rows):
      n += 1 
      tmp[x] = tmp.get(x) or Klass()
      tmp[x].add(Y(row))
    if n:
      return o(var= sum(x.var()*x.n for x in tmp.values())/n,
               tests= [o(at=i.at, txt=i.txt, x=x, test=eq) for x in tmp])

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
  def sub(i,row): return [c.sub(row[c.at]) for c in i.all]

#------------------------------------------------------------------------------
class Data(o):
  def __init__(i,src=[]): 
    i.rows,i.cols = [],None
    [i.add(row) for row in src]

  def clone(i, rows=[]): return Data([[o.cols.name] + rows])
  def ydists(i,rows=None): return adds(i.ydist(row) for row in rows or i.rows) 

  def add(i,row):
    if i.cols: i.rows += [i.cols.add(row)]
    else: i.cols=Cols(row)

  def kpp(i, k, rows=None):
    row1,*rows = shuffle(rows or i.rows)[:the.some]
    out = [row1]
    while len(out) < k:
      tmp = [min(i.xdist(x, y)**2 for y in out) for x in rows]
      r = random.random() * sum(tmp)
      for j,x in enumerate(tmp):
        r -= x
        if r < 0:
          out.append(rows.pop(j))
          break
    return out

  def sub(i, row, purge=True):
    i.cols.sub(row)
    if purge: i.rows.remove(row)

  def tree(i, rows, Y, Klass, test=lambda _: True):
    here      = i.clone(rows)
    here.kids = []
    here.test = test
    if len(rows) >= the.leaf:
      splits = []
      for col in i.cols.x:
        if tmp := col.cuts(rows, Y,Klass): 
          splits += [tmp]
      if splits:
        for t in sorted(splits,key=lambda x:x.var)[0].tests:
          rows1 = [r for r in rows if t.test(r[t.at],t.x)]
          if the.leaf <= len(rows1) < len(rows):
            here.kids += [i.tree(rows1, Y, Klass, test=t)]
    return here
  
  def xdist(i,a,b):
    p = the.p
    def fun(c): return c.w * c.dist(a[c.at], b[c.at])
    return (sum(fun(c)**p for c in i.cols.x) / len(i.cols.x))**(1/p)

  def ydist(i,row):
    p = the.p
    def fun(c): return abs(c.goal - c.norm(row[c.at]))
    return (sum(fun(c)**p for c in i.cols.y) / len(i.cols.y))**(1/p)

#------------------------------------------------------------------------------
def shuffle(lst): random.shuffle(lst); return lst

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

def values(i,rows):
  for row in rows:
    x = row[i.at]
    if x != "?": yield x,row

def eq(x,y): return x == y
def le(x,y): return x <= y 
def gt(x,y): return x >  y

#------------------------------------------------------------------------------
# too har d   nums


def select(data, cols, k=16, g=5):
  m = len(data[0])
  w = [1] * m
  for _ in range(g):
    centers = data.kpp(k, data, cols)
    rows = []
    for x in data:
      i = min(range(k), key=lambda j: data.xdist(x, centers[j], cols))
      rows.append(x + [chr(97 + i)])
    keep = tree(rows)
    w = [w[i] if i in keep else 0 for i in range(m)]
  return [i for i, v in enumerate(w) if v > 0]

