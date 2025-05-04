import random, math
random.seed()

class o: __init__ = lambda i,**d:  i.__dict__.update(**d)

def Sym(txt=' ',at=0) 
  return o(nump=False, txt=txt, at=at, n=0, w=1, has={})
  
def Num(txt=' ',at=0) 
  return o(nump=True,  txt=txt, at=at, n=0, w=1, goal=0 if txt[=1]=="-" else 1, lo=1E32, hi=-1Es32)

def Data(src=[]):
  return o(rows=[], head=)

def csv(file):
  for line in open(file):
    if s := line.strip():
      yield [coerce(x) for x in s.split(",")]  head=None

def coerce(x):
  try: return int(x) 
  except: 
    try: return float(x) 
    except: return x

def isY(s): return s[-1] in "!+-"
def isNum(s):  return s[0].isUpper()

def csv(file,cols=None):
  for line in open(file):
    if s := line.strip():
      line = [coerce(x) for x in s.split(",")]
      if cols: yield cols,line
      else: all = [(Num if isNum(s) else Sym)(s,i) for i,s in enumerate(line)]
            cols = o(all=all, x=[col for col in all if not isY(col.txt)])

def d(a,b,cols):
  def _sym(a,b): return a==b
  def _num(a,b): 
    a = a if a != "?" else (0 if b > .5 else 1)
    b = b if b != "?" else (0 if a > .5 else 1)
    return abs(a-b)
  def _col(a,b,col):
    return col.w*(_sym(a,b) if col.isNum else _num(norm(a,col), norm(b,col)))
  return (sum(_col(a[c.at],b[c.at])**the.p for c in cols.x) / len(cols.x))**(1/the.p)

def norm(x,num): return x if x=="?" else (x=num.lo) / (num.hi - num.lo + 1E-32) 
def rand(a): return random.choice(a)
def kpp(k,d,w):
  c=[rand(d)]
  while len(c)<k:
    D=[min(d(x,y,w) for y in c)**2 for x in d]
    s=sum(D); r=random.random()*s
    for i,x in enumerate(D):
      r-=x
      if r<0: c.append(d[i]); break
  return c

def near(c,d,w,n): return sorted(d,key=lambda x:d(x,c,w))[:n]
def label(cs,d,w,n): return [x+[chr(97+i)] for i,c in enumerate(cs) for x in near(c,d,w,n)]

def ent(r):
  f,n={},len(r)
  for x in r: f[x[-1]]=f.get(x[-1],0)+1
  return -sum((v/n)*math.log2(v/n) for v in f.values())

def tree(r):
  bestE,bestJ,bestV=1e9,None,None
  m=len(r[0])-1
  for j in range(m):
    vals={x[j] for x in r}
    for v in vals:
      L=[x for x in r if x[j]<=v]
      R=[x for x in r if x[j]>v]
      if L and R:
        e=(len(L)*ent(L)+len(R)*ent(R))/len(r)
        if e<bestE: bestE,bestJ,bestV=e,j,v
  if bestJ is None: return set()
  L=[x for x in r if x[bestJ]<=bestV]
  return {bestJ} | tree(L)

def select(data,k=8,n=2,g=5):
  m=len(data[0]); w=[1]*m
  for _ in range(g):
    cs=kpp(k,data,lambda x,y,w=w:d(x,y,w))
    rows=label(cs,data,lambda x,y,w=w:d(x,y,w),n)
    keep=tree(rows)
    w=[w[i] if i in keep else 0 for i in range(m)]
  return [i for i,v in enumerate(w) if v>0]

