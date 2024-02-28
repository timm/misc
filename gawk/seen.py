# seen.py  : tiny ai teaching lab
# (c)2024, Tim Menzies, BSD2 license. Share and enjoy.
import random,math,ast,sys,re
from fileinput import FileInput as file_or_stdin

config = dict(  begin=4,
                budget=15,
                enough=0.5,
                file="../data/auto93.csv",
                k=1,
                m=2,
                seed=1234567891,
                todo="the")
#----------------------------------------------------------------------------------------
class OBJ:
  def __init__(i,**d): i.__dict__.update(d)
  def __repr__(i): return i.__class__.__name__+'{'+show(i.__dict__)+'}'

the  = OBJ(**config)
big  = 1E30
tiny = 1/big
isa  = isinstance
r    = random.random

def adds(x,lst): [x.add(y) for y in lst]; return x

def cli(d):
  for k,v in d.items():
    for c,arg in enumerate(sys.argv):
      after = "" if c >= len(sys.argv) - 1 else sys.argv[c+1]
      if arg in ["-"+k[0], "--"+k]:
        v = str(v)
        v = "false" if v==True else ("true" if v==False else after)
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

def show(x,n=2):
  if isa(x,(int,float)) : return x if int(x)==x else round(x,n)
  if isa(x,(list,tuple)): return [show(y,n) for y in x][:10]
  if isa(x,dict): 
    return ' '.join(f":{k} {show(v,n)}" for k,v in x.items() if k[0]!="_")
  return x
#----------------------------------------------------------------------------------------
class COL(OBJ):
  def __init__(i,at=0,txt=" "):
    i.n,i.at,i.txt = 0,at,txt
    i.heaven = 0 if txt[-1]=="-" else 1

class SYM(COL):
  def __init__(i,**d)  : super().__init__(**d); i.has={}
  def add(i,x)         : i.n += 1; i.has[x] = 1 + i.has.get(x,0)
  def like(i,x,m,prior): return (i.has.get(x, 0) + m*prior) / (i.n + m)
  def mid(i)           : return max(i.has, key=i.has.get)

  def div(i):
    return -sum(n/i.n * math.log(n/i.n,2) for n in i.has.values() if n > 0)

class NUM(COL):
  def __init__(i,**d): super().__init__(**d); i.mu,i.m2,i.lo,i.hi = 0,0,big,-big
  def div(i)         : return 0 if i.n < 2 else (i.m2 / (i.n - 1))**.5
  def mid(i)         : return i.mu
  def norm(i,n)      : return n=="?" and n or (n - i.lo) / (i.hi - i.lo + tiny)

  def add(i,n):
    i.n += 1
    i.lo = min(n,i.lo)
    i.hi = max(n,i.hi)
    delta = n - i.mu
    i.mu += delta / i.n
    i.m2 += delta * (n -  i.mu)

  def like(i,n,*_):
    v     = i.div()**2 + tiny
    nom   = math.e**(-1*(n - i.mid())**2/(2*v)) + tiny
    denom = (2*math.pi*v)**.5
    return min(1, nom/(denom + tiny))

class COLS(OBJ):
  def __init__(i,names):
    i.x,i.y,i.all,i.names,i.klass = [],[],[],names,None
    for at,txt in enumerate(names):
      a,z = txt[0], txt[-1]
      col = (NUM if a.isupper() else SYM)(at=at,txt=txt)
      i.all.append(col)
      if z != "X":
        (i.y if z in "!+-" else i.x).append(col)
        if z == "!": i.klass= col

  def add(i,lst): 
    [col.add(lst[col.at]) for col in i.all if lst[col.at] != "?"]; return lst

class DATA(OBJ):
  def __init__(i,src=[],fun=None,ordered=False):
    i.rows, i.cols = [],[]
    [i.add(lst,fun) for lst in src]
    if ordered: i.ordered()

  def add(i,lst,fun=None):
    if i.cols:
      if fun: fun(i,lst)
      i.rows += [i.cols.add(lst)]
    else: i.cols = COLS(lst)

  def clone(i): return DATA([i.cols.names])

  def loglike(i, lst, nall, nh, m,k):
    prior = (len(i.rows) + k) / (nall + k*nh)
    likes = [c.like(lst[c.at],m,prior) for c in i.cols.x if lst[c.at] != "?"]
    return sum(math.log(x) for x in likes + [prior] if x>0)

# test cloning, sorting on d2h
  def smo(i, score=lambda B,R: B/(R+tiny)):
    def p(lst,data): 
      return data.loglike(lst,len(i.rows),2,the.m,the.m)
    def acquire(best, rest, rows):
      return max((score(p(r,best), p(r,rest)),j) for j,r in enumerate(rows)])[1]
    #---------------------
    random.shuffle(i.rows)
    done, todo = i.rows[:the.begin], i.rows[the.begin:]
    data1 = i.clone(done, order=True)
    for _ in range(the.budget):
      n = int(len(done)**the.enough + .5)
      done.append(
        todo.pop( 
          acquire(i.clone(data1.rows[:n]), i.clone(data1.rows[n:]), todo)))
      data1 = i.clone(done, order=True)
    return data1.rows[0]

class NB(OBJ):
  def __init__(i): i.correct,i.nall,i.datas = 0,0,{}

  def loglike(i,data,lst):
    return data.loglike(lst, i.nall, len(i.datas), the.m, the.k)

  def run(i,data,lst):
    klass = lst[data.cols.klass.at]
    i.nall += 1
    if i.nall > 10:
      guess = max((i.loglike(data,lst),klass1) for klass1,data in i.datas.items())
      i.correct += klass == guess[1] 
    if klass not in i.datas: i.datas[klass] =  data.clone()
    i.datas[klass].add(lst)

  def report(i): return OBJ(accuracy = i.correct / i.nall)

#----------------------------------------------------------------------------------------
class eg:
  def unknown(): print(f"W> unknown action [{the.todo}].")
  
  def the():  print(the)

  def sym():
    s = adds(SYM(),"aaaabbc")
    assert round(s.div(),2) == 1.38 and s.mid() == "a" 

  def one():
    w = OBJ(n=0)
    def inc(_,r): w.n += len(r)
    d = DATA(csv("../data/auto93.csv"), inc) 
    assert w.n == 3184

  def nb():
    out=[]
    for k in [1,2,3]:
      for m in [1,2,3]: 
        the.k, the.m = k,m
        nb = NB()
        DATA(csv("../data/soybean.csv"), nb.run)
        out += [OBJ(acc = nb.report().accuracy, k=k, m=m)]
    [print(show(x,3)) for x in sorted(out,key=lambda z: z.acc)]

#----------------------------------------------------------------------------------------
if __name__=="__main__":
  the = OBJ(**cli(config))
  random.seed(the.seed)
  getattr(eg, the.todo, eg.unknown)()