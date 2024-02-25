import random,math,ast,sys,re
from fileinput import FileInput as file_or_stdin

class OBJ:
  def __init__(i,**d): i.__dict__.update(d)
  def __repr__(i)    : return i.__class__.__name__+str(i.__dict__)

defaults = dict(seed=1234567891,
                k=1,
                m=2,
                file="../data/auto93.csv")

the  = OBJ(**defaults)
big  = 1E30
tiny = 1/big
r    = random.random

def adds(x,lst): [x.add(y) for y in lst]; return x

def cli(d):
  for k,v in d.items():
    for c,arg in enumerate(sys.argv):
      after = "" if c >= len(sys.argv) - 1 else sys.argv[c+1]
      if arg in ["-"+k[0], "--"+k]:
        v = "false" if v==true else ("true" if v==false else after)
        d[k] = coerce(v)
  return d

def coerce(s):
  try: return ast.literal_eval(s)
  except Exception: return s

def csv(file=None):
  with file_or_stdin(file) as src:
    for line in src:
      line = re.sub(r'([\n\t\r"\’ ]|#.*)', '', line)
      if line: yield [coerce(s.strip()) for s in line.split(",")]
#-------------------------------------------------
class COL(OBJ):
  def __init__(i,at=0,txt=" "):
    i.n,i.at,i.txt = 0,at,txt
    i.heaven = 0 if txt[-1]=="-" else 1

class SYM(COL):
  def __init__(i,**d):   super().__init__(**d); i.has={}

  def add(i,x): i.has[x] = i.has.get(x,0) + 1

  def div(i,x):
    return -sum(n/i.n * math.log(n/i.n,2) for n in i.has.values() if n > 0)

  def like(i,x,m,prior): return (i.has.get(x, 0) + m*prior) / (i.n + m)

  def mid(i,x): return max(i.has, key=i.has.get)

class NUM(COL):
  def __init__(i,**d):
    super().__init__(**d)
    i.mu, i.m2, i.lo, i.hi = 0,0, big, -big

  def add(i,x):
    i.n += 1
    i.lo = min(x,i.lo)
    i.hi = max(x,i.hi)
    delta = x - i.mu
    i.mu += delta / i.n
    i.m2 += delta * (x -  i.mu)

  def div(i,x): return 0 if i.n < 2 else (i.m2 / (i.n - 1))**.5

  def like(i,x,*_):
    v     = i.div()**2 + tiny
    nom   = math.e**(-1*(x - i.mid())**2/(2*v)) + tiny
    denom = (2*math.pi*v)**.5
    return min(1, nom/(denom + tiny))

  def mid(i,x): return i.mu

  def norm(i,x): return x=="?" and x or (x - i.lo) / (i.hi - i.lo + tiny)

class COLS(OBJ):
  def __init__(i,names):
    i.x,i.y,i.all,i.names,i.klass = [],[],[],names,None
    for at,txt in enumerate(names):
      a,z = txt[0], txt[-1]
      col = (NUM if a.isupper() else SYM)(at,txt)
      i.all.append(col)
      if z != "X":
        (i.y if z in "!+-" else i.x).append(col)
        if z == "!" then i.klass= col

  def add(i,row):
    [col.add(row[col.at]) for col in i.all if row[col.at] != "?"]
    return row

class DATA(OBJ):
  def __init__(i,src=[],fun=None.ordered=False):
    i.rows, i.cols = [],[]
    adds(i, src)
    if ordered: self.ordered()

  def add(i,src,fun=None):
    if i.cols:
      if fun: fun(i,row)
      i.rows += [i.cols.add(row)]
    else: i.cols = COLS(row)

  def clone(i): return DATA([i.cols.names])

  def loglike(i, row, nall, nh, m=the.m, k=the.k):
    prior = (len(i.rows) + k) / (nall + k*nh)
    likes = [c.like(row[c.at],m,prior) for c in i.cols.x if row[c.at] != "?"]
    return sum(math.log(x) for x in likes + [prior])
#-------------------------------------------------
class eg:
  def one(): 
    d=DATA(csv("../data/auto93.csv"))
    print(d.cols.all[2].has)

  def nb(file = the.src): # make this a class
    y,n,datas = 0,0,{}
    def fun(data,row):
      n += 1
      kl = row[data.cols.klass]
      if n > 10:
        guess = max([(v.loglike(row,n,2),k) for k,v in datas.items()])[1]
        if kl = guess: y += 1
      if kl not in datas: datas[kl] =  data.clone()
      datas[kl].add(row)
    DATA(csv(file),fun=fun)
    print(my.y/my.n)

if __name__=="__main__" and len(sys.argv)>1:
    the = obj(**cli(defaults))
	random.seed(the.seed)
	getattr(eg, sys.argv[1], lambda: print("?"))()
