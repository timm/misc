import random,ast,sys,re
from fileinput import FileInput as file_or_stdin

class obj:
    defaults=dict(seed=1234567891,
                k=1,
                m=2,
                file="../data/auto93.csv"
             )
  def __init__(i,**d): i.__dict__.update(d)
  def __repr__(i): return i.__class__.__name__+str(i.__dict__)

#-------------------------------------------------
class Some(obj):
  def __init__(i): i.n,i.ok,i._has = 0,False,[]
  def add(i,x): 
    i.n += 1
    if   len(i._has) < the.some : i.ok=False; i._has += [x]
    elif r() < the.some/i.n     : i.ok=False; i._has[int(len(i._has)*r())] = x 
  def mid(i): a=i.has; return a[int(len(a)*.5)]
  def div(i): a=i.has; return (a[int(len(a)*.9)] - a[int(len(a)*.1)])/2.56
  @property
  def has(i):
    if not i.ok: i._has.sort(); i.ok=True
    return i._has

class COLS(obj):
  def __init__(i,names):
    i.names = names
    i.all = {c:(Some() if s[0].isupper() else {}) for c,s in  enumerate(names)}
    i.y   = {c:i.all[c] for c,s in enumerate(names) if s[-1] in "!+-"}
    for c,s in enumerate(names):
      if s[-1] == "!": i.klass= c

  def add(i,row):
    for c,col in i.all.items():
      x = row[c]
      if x != "?":
        if isa(col,Some): col.add(x)
        else: col[x] = col.get(x,0) + 1
    return row

class DATA(obj):
  def __init__(i,src=[],fun=None.ordered=False):
    i.rows, i.cols=[],[]
    i.adds(src,fun)
    if ordered: self.ordered()

  def clone(i): return DATA([i.cols.names]) 

  def adds(i,src,fun=None:
    for row in src: 
      if i.cols: 
        if fun: fun(i,row)
        i.rows += [i.cols.add(row)]
      else: i.cols = COLS(row)

  def loglike(i,row,nall,nh,m=the.m,k=the.k):
    def num(col,x):
      v     = col.some.div()**2 + tiny
      nom   = math.e**(-1*(x - col.some.mid())**2/(2*v)) + tiny
      denom = (2*math.pi*v)**.5
      return min(1, nom/(denom + tiny))
    def sym(col,x):
      return (col.get(x, 0) + m*prior) / (len(i.rows) + m)
    #------------------------------------------
    prior = (len(i.rows) + k) / (nall + k*nh)
    out   = math.log(prior)
    for c,col in enumerate(i.cols.all.items()):
      x = row[c]
      if x != "?" and c not in self.ys:
        out += math.log((sym if isa(col,Sym) else num)(col, x))
    return out
#-------------------------------------------------
r=random.random
isa=isinstance

def coerce(s):
  try: return ast.literal_eval(s)
  except Exception: return s

def csv(file=None):
  with file_or_stdin(file) as src:
    for line in src:
      line = re.sub(r'([\n\t\r"\’ ]|#.*)', '', line)
      if line: yield [coerce(s.strip()) for s in line.split(",")]
#-------------------------------------------------
class eg:
  def one(): 
    d=DATA(csv("../data/auto93.csv"))
    print(d.cols.all[2].has)

  def nb(file = the.src):
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

the=obj(**obj.defaults)
if __name__=="__main__" and len(sys.argv)>1:
	random.seed(the.seed)
	getattr(eg, sys.argv[1], lambda: print("?"))()
