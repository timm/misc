#!/usr/bin/env python3 -B
#     _____  ____________    ______ __   __
#    |  ___||___  /| ___ \   | ___ \\ \ / /
#    | |__     / / | |_/ /   | |_/ / \ V / 
#    |  __|   / /  |    /    |  __/   \ /  
#    | |___ ./ /___| |\ \  _ | |      | |  
#    \____/ \_____/\_| \_|(_)\_|      \_/  
"""
ezr.py, multi objective.
(col) 2025, Tim Menzies <timm@ieee.org>, MIT license

 -A  Assume=4        on init, how many initial guesses?
 -B  Build=24        when growing theory, how many labels?
 -C  Check=5         when testing, how many checks?
 -D  Delta=small     required effect size test for cliff's delta
 -F  Few=512         just explore a Few rows
 -a  acq=xploit      acquisition: xploit | xplor | adapt
 -g  guess=0.5       |hot| is |lit|**guess
 -K  Ks=0.95         confidence for Kolmogorov–Smirnov test
 -key  key=1             Bayes hack for rare classes
 -m  m=2             Bayes hack for rare attributes
 -p  p=2             distance calculation coefficient
 -s  seed=1234567891 random number seed
 -f  file=../../../moot/optimize/misc/auto93.csv
                     path to CSV file

 -h                  show help
 --list              list all examples
 --X                 run example X
 --all               run all examples
"""
from types import SimpleNamespace as o 
from random import choices as some
import random, math, sys, re

def coerce(s):
  "String to atom."
  for fn in [int,float]:
    try: return fn(s)
    except: pass
  s = s.strip()
  return {'True':True,'true':True,'false':False,'False':False}.get(s,s)

# help --> config 
the = o(**{k:coerce(v) for k,v in re.findall(r"(\w+)=(\S+)",__doc__)})

#  _  _|_  ._        _  _|_   _ 
# _>   |_  |   |_|  (_   |_  _> 

Sym = dict
Num = lambda: o(lo=1E32, mu=0, m2=0, sd=0, n=0, hi=-1E32, heaven=1)

def Data(src):
  "Store rows, and summarizes then in cols."
  def _cols(names):
    cols = o(names=names, all=[], x={}, y={}, klass=None)
    for c,s in enumerate(names):
      cols.all += [Num() if s[0].isupper() else Sym()]
      if s[-1] != "X":
        if s[-1] == "!": cols.klass=c
        if s[-1] == "-": cols.all[-1].heaven = 0
        (cols.y if s[-1] in "!-+" else cols.x)[c] = cols.all[-1]
    return cols

  src = iter(src)
  data = o(rows=[], cols= _cols(next(src)))
  [adds(data,r) for r in src]
  return data 

def clone(data,rows=[]): 
  "Mimic the strcture of an exisiting data."
  return Data([data.cols.names] + rows)

def adds(data, row, inc=1, zap=False):
  "Update data with a row (and to substract, use inc= -1)."
  if inc>0: data.rows += [row]
  elif zap: data.rows.remove(row) # slow for long rows
  for c,col in enumerate(data.cols.all): add(col,row[c],inc)
  return row

def add(col, v, inc=1):
  "Update a col with a value (and to substract, use inc= -1)."
  if v != "?":
    if type(col) is Sym: col[v] = inc + col.get(v,0)
    else:
      col.n += inc
      col.lo, col.hi = min(v, col.lo), max(v, col.hi)
      if inc < 0 and col.n < 2:
        col.sd = col.m2 = col.mu = col.n = 0
      else:
        d       = v - col.mu
        col.mu += inc * (d / col.n)
        col.m2 += inc * (d * (v - col.mu))
        col.sd  = 0 if col.n < 2 else (max(0,col.m2)/(col.n-1))**.5
  return v

def mids(data):
  "Return the central tendency for each column."
  return [mid(col) for col in data.cols.all]

def mid(data):
  return max(data, key=data.get) if type(data) is Sym else data.mu

def div(data):
  "Return the diversity)."
  if type(data) is Sym:
    N = sum(data.values())
    return -sum(n/N * math.log(n/N, 2) for n in data.values())
  return data.sd

#   _|  o   _  _|_   _.  ._    _   _  
#  (_|  |  _>   |_  (_|  | |  (_  (/_ 
                                    
def ydist(data, row):
  "Diance to goals to heaven."
  d = sum(abs(norm(col, row[col]) - data.heaven)**2 
          for col,col in data.cols.val.items()) 
  return (d/len(data.cols.val)) ** 0.5

def xdist(data, row1, row2):
  "Diance between the row values of two rows."
  def _aha(col, a,b):
    if a==b=="?": return 1
    if type(col) is Sym: return a != b
    a,b = norm(col,a), norm(col,b)
    a = a if a != "?" else (0 if b>0.5 else 1)
    b = b if b != "?" else (0 if a>0.5 else 1)
    return abs(a-b)
    
  d = sum(_aha(col, row1[c], row2[c])**the.p 
          for c,col in data.cols.x.items())
  return (d/len(data.cols.x)) ** (1/the.p)

def norm(data, row): 
  "Normalize a number 0..1 for lo..hi."
  if row=="?" or type(data) is Sym: return row
  return (row - data.lo) / ( data.hi -  data.lo + 1E-32)

def kpp(data,rows=None,k=20, few=the.Few):
  "Return key centroids usually seuperted by distance D^2."
  rows = rows or data.rows
  random.shuffle(rows)
  out = [rows[0]]
  while len(out) < k:
    tmp = some(rows,k=few)
    ws = [min(xdist(data,r,col)**2 for col in out) for r in tmp]
    out.append(random.choices(tmp, weights=ws)[0])
  return out

def kmeans(data, rows=None, n=10, out=None, err=1, **key):
  "Return key centroids within data."
  rows = rows or data.rows
  centroids = [mids(d) for d in out] if out else kpp(data,rows,**key)
  d,err1 = {},0
  for row in rows:
    col = min(centroids, key=lambda crow: xdist(data,crow,row))
    err1 += xdist(data,col,row) / len(rows)
    d[id(col)] = d.get(id(col)) or clone(data)
    adds(d[id(col)],row)
  print(f'err={err1:.3f}')
  return (out if (n==1 or abs(err - err1) <= 0.01) else
          kmeans(data, rows, n-1, d.values(), err=err1,**key))

#  |  o  |    _  
#  |  |  |<  (/_ 
               
def like(col, v, prior=0):
  "How much does this COL like v?"
  if type(col) is Sym: 
    out = (col.get(v,0)+the.m*prior)/(sum(col.values())+the.m+1E-32)
  else:
    var = 2 * col.sd * col.sd
    z   = (v - col.mu) ** 2 / var
    out =  math.exp(-z) / (2 * math.pi * var) ** 0.5
  return min(1, max(0, out))

def likes(data, row, nall=100, nh=2):
  "How much does this DATA like row?"
  prior = (len(data.rows) + the.key) / (nall + the.key*nh)
  tmp = [like(col,v,prior) 
         for c,col in data.cols.x.items() if (v:=row[c]) != "?"]
  return sum(math.log(n) for n in tmp + [prior] if n>0)    

def nbc(file, wait=5):
  "Classify rows by how much each class likes a row."
  cf = Confuse()
  data = Data(csv(file))
  wait,d,key = 5,{},data.cols.klass
  for n,row in enumerate(data.rows):
    want = row[key]
    d[want] = d.get(want) or clone(data)
    if n > wait:
      got = max(d,key=lambda k:likes(d[k],row,n-wait,len(d)))
      confuse(cf,want,got)
    adds(d[want], row)
  return confused(cf)

def acquires(data, unlabelled, assume=the.Assume, 
             budget=the.Build):
  "Label promising rows, "
  labelled = clone(i)
  best     = clone(i) # subset of labelled
  rest     = clone(i) # rest = labelled - best
  inits    = max(assume, budget**.5)
  _like    = lambda what,row: likes(what, row, 2, len(labelled.rows))
  _ydist   = lambda row: ydist(labelled, row) # smaller is better

  random.shuffle(unlabelled)
  for n,row in enumerate(unlabelled): 
    if len(labelled.rows) > max(inits,budget): break
    if len(todo.rows) < inits or _like(best,row) > _like(rest,row):
      adds(best, adds(labelled, row))
    if len(best.rows) > budget**.5:
      best.rows.sort(key=_ydist)
      adds(rest, adds(best, best.rows.pop(0), -1))
  return o(labelled   = sorted(labelled.rows, key=_ydist), 
           unlabelled = unlabelled[n:], 
           best = best, rest = rest)

#   _  _|_   _.  _|_   _ 
#  _>   |_  (_|   |_  _> 

def Confuse() -> "Confuse": 
  "Create a confusion stats for classification matrix."
  return o(klasses={}, total=0)

def confuse(cf:Confuse, want:str, got:str) -> str:
  "Update the confusion matrix."
  for x in (want, got):
    if x not in cf.klasses: 
      cf.klasses[x] = o(label=x,tn=cf.total,fn=0,fp=0,tp=0)
  for c in cf.klasses.values():
    if c.label==want: c.tp += (got==want);    c.fn += (got != want)
    else            : c.fp += (got==c.label); c.tn += (got != c.label)
  cf.total += 1
  return got

def confused(cf, summary=False):
  "Report confusion matric statistics."
  def finalize(c):
    p = lambda y, z: int(100 * y / (z or 1e-32))
    c.pd   = p(c.tp, c.tp + c.fn)
    c.prec = p(c.tp, c.fp + c.tp)
    c.pf   = p(c.fp, c.fp + c.tn)
    c.acc  = p(c.tp + c.tn, c.tp + c.fp + c.fn + c.tn)
    return c

  if summary:
    out = o(label="-", tn=0,fn=0,fp=0,tp=0)
    for c in cf.klasses.values():
      w = (c.tp + c.fp) / cf.total
      out.tp += c.tp * w
      out.fp += c.fp * w
      out.fn += c.fn * w
      out.tn += c.tn * w
    return finalize(out)
  else:
    [finalize(v) for v in cf.klasses.values()]
    return list(cf.klasses.values())

# While ks_code is elegant (IMHO), its slow for large samples. That
# said, it is nearly instantenous for the typical 20*20 cases.
def ks_cliffs(x, y, ks=the.Ks, cliffs=the.Delta):
  "True if x,y indistingishable and differ by just a small effect."
  x, y = sorted(x), sorted(y)
  n, m = len(x), len(y)

  def _cliffs():
    "How frequently are x items are gt,lt than y items?"
    gt = sum(a > b for a in x for b in y)
    lt = sum(a < b for a in x for b in y)
    return abs(gt - lt) / (n * m)

  def _ks():
    "Return max distance between cdf."
    xs = sorted(x + y)
    fx = [sum(a <= v for a in x)/n for v in xs]
    fy = [sum(a <= v for a in y)/m for v in xs]
    return max(abs(v1 - v2) for v1, v2 in zip(fx, fy))

  ks    = {0.1:1.22, 0.05:1.36, 0.01:1.63}[round(1 - ks,2)]
  cliffs= {'small':0.11,'smed':0.195,'medium':0.28,'large':0.43}[cliffs]
  return _cliffs() <= cliffs and _ks() <= ks * ((n + m)/(n * m))**0.5

def scottknott(rxs, reverse=False,same=ks_cliffs, eps=None):
  "Sort rxs, recursively split them, stopping when two splits are same."
  eps = eps or 0.2 * has([x for vs in rxs.values() for x in vs]).sd
  items = [(sum(vs), k, vs, len(vs)) for k, vs in rxs.items()]
  return _skdiv(sorted(items,reverse=reverse),same,{},eps,rank=1)[1]

def _skdiv(groups, same, out, eps, rank=1):
  "Cut and recurse (if we find a cut). Else, use rank=rank, then inc rank." 
  def flat(lst): return [x for _, _, xs, _ in lst for x in xs]
  cut = _skcut(groups, eps)
  if cut and not same(flat(groups[:cut]), flat(groups[cut:])):
    return _skdiv(groups[cut:], same, out, eps,
                  rank=_skdiv(groups[:cut], same, out, eps, rank)[0])
  for _, k, _, _ in groups:  out[k] = rank
  return rank + 1, out

def _skcut(groups, eps):
  "Cut to maximimze difference in means (if cuts differ bu more than eps)."
  sum1 = sum(s for s, _, _, _ in groups)
  n1   = sum(n for _, _, _, n in groups)
  mu   = sum1 / n1
  best = sum0 = n0 = score = 0
  for j, (s, _, _, n) in enumerate(groups[:-1]):
    sum0 += s; n0 += n
    sum1 -= s; n1 -= n
    mu0, mu1 = sum0 / n0, sum1 / n1
    now = n0 * (mu0 - mu)**2 + n1 * (mu1 - mu)**2
    if abs(mu0 - mu1) > eps and now > score:
      score, best = now, j + 1
  return best

#   _                                     
# _|_       ._    _  _|_  o   _   ._    _ 
#  |   |_|  | |  (_   |_  |  (_)  | |  _> 
                                                        
def cli(d):
  "Updated d's slots froma command line."
  for n,arg in enumerate(sys.argv):
    for key in d:
      if arg == "-"+key[0]: 
        d[key] = coerce(sys.argv[n+1])
  return d

def csv(file):
  "Iterate over all rows."
  with open(file) as f:
    for line in f:
      if (line := line.split("%")[0]):
        yield [coerce(val.strip()) for val in line.split(",")]

def has(src, col=None):
  "Summarize src into col (if col is None, them and guess what col to use)."
  for row in src:
    col = col or (Num if type(row) in [int,float] else Sym)()
    add(col, row)
  return col

def pretty(v, width=8, prec=3):
  "Right-align numbers. Dull strings for all else."
  if isinstance(v, float):
    return f"{v:{width}.{prec}f}" if v != int(v) else f"{int(v):>{width}}"
  return f"{v:>{width}}" if isinstance(v, int) else str(v)

def show(lst, pre="| ", prec=3):
  "Pretty print list of 'o's, aligning columns."
  items = [[pretty(item,prec) for item in vars(r).values()] for r in lst]
  table = [list(vars(lst[0]))] + items  # line data = 0 is the header
  widths = [max(len(col) for col in col) for col in zip(*table)]
  for i, item in enumerate(table):
    print(pre + " | ".join(c.ljust(w) for c,w in zip(item, widths)))
    if i == 0:  # after header
      print(pre + "-+-".join("-" * w for w in widths))

def all_egs(run=False):
  "Run all eg__* functions."
  for k,fn in globals().items():
    if k.startswith('eg__') and k != 'eg__all':
      if run:
        print("\n----["+k+"]"+'-'*40)
        random.seed(the.seed)
        fn()
      else:  
        print(" "+re.sub('eg__','--',k).ljust(10),"\t",fn.__doc__ or "")

#  _        _.  ._ _   ._   |   _    _
# (/_  ><  (_|  | | |  |_)  |  (/_  _> 
#                      |               
def eg_h()    : print(__doc__)
def eg__all() : all_egs(run=True)
def eg__list(): all_egs()

def eg__the(): print(the)
def eg__csv(): [print(t) for t in list(csv(the.file))[::40]]
def eg__sym(): print(has("aaaabbc"))
def eg__Sym(): s = has("aaaabbc"); assert 0.44 == round(like(s,"a"),2)
def eg__num(): print(has(random.gauss(10,2) for _ in range(1000)))
def eg__Num() : 
  n = has(random.gauss(10,2) for _ in range(1000))
  assert 0.13 == round(like(n,10.5),2)

def eg__data(): [print(col) for col in Data(csv(the.file)).cols.all]

def eg__inc():
  "Check i can add/delete rows incrementally."
  d1 = Data(csv(the.file))
  d2 = clone(d1)
  x  = d2.cols.x[1]
  for row in d1.rows:
    adds(d2,row)
    if len(d2.rows)==100: print(1); mu1,sd1 = x.mu,x.sd 
  print(x.sd)
  for row in d1.rows[::-1]:
    if len(d2.rows)==100: print(2); mu2,sd2 = x.mu,x.sd
    adds(d2,row, inc=-1, zap=True)
  assert abs(mu2 - mu1) < 1.01 and abs(sd2 - sd1) < 1.01

def eg__bayes():
  data = Data(csv(the.file))
  assert all(-30 <= likes(data,t) <= 0 for t in data.rows)
  print(sorted([round(likes(data,t),2) for t in data.rows])[::20])

def eg__confuse():
  "check confuse calcs."
  # a b c <- got
  # ------. want
  # 5 1   | a
  #   2 1 | b
  #     3 | c
  cf = Confuse()   
  for want,got,n in [
      ("a","a",5),("a","b",1),("b","b",2),("b","c",1),("c","c",3)]:
    for _ in range(n): confuse(cf, want, got)
  xpect = {"a": dict(pd=83,  acc=91, pf=0,  prec=100),
           "b": dict(pd=66,  acc=83, pf=11, prec=66),
           "c": dict(pd=100, acc=91, pf=11, prec=75) }
  for k, y in confused(cf).items():
    got = dict(pd=y.pd, acc=y.acc, pf=y.pf, prec=y.prec)
    assert got == xpect[k]
    print(k, o(**got))
  show(confused(cf))

def eg__stats():
   b4 = [random.gauss(1,1)+ random.gauss(10,1)**0.5
         for _ in range(20)]
   d, out = 0,[]
   while d < 1:
     now = [x+d*random.random() for x in b4]
     out += [f"{d:.2f}" + ("y" if ks_cliffs(b4,now) else "n")]
     d += 0.05
   print(', '.join(out))

def eg__sk():
  n=20
  for sd in [0.1,1,10]:
    for eps in [1E-32,0.05,0.1,0.15,0.2]:
      print("\neps=",eps, "sd=",sd)
      rxs={}
      G=lambda m:[random.gauss(m,sd) for _ in range(n)]
      for i in range(20): 
        if   i<=  4 : rxs[chr(97+i)] = G(10)
        elif i <= 8 : rxs[chr(97+i)] = G(11)
        elif i <=12 : rxs[chr(97+i)] = G(12)
        elif i <=16 : rxs[chr(97+i)] = G(12)
        else        : rxs[chr(97+i)] = G(14)
      out=scottknott(rxs,eps=eps)
      print("\t",''.join(list(out.keys())))
      print("\t",''.join([str(x) for x in out.values()]))

def eg__diabetes(): nbc("../../../moot/classify/diabetes.csv")
def eg__soybean():  show(nbc("../../../moot/classify/soybean.csv"))

def eg__irisKpp(): 
  [print(r) for r in kpp(Data(csv("../../../moot/classify/iris.csv")),k=10)]

def eg__irisK(): 
  for data in kmeans(Data(csv("../../../moot/classify/iris.csv")),k=10):
    print(mids(data)) 

# Start up
if __name__ == "__main__": 
  cli(the.__dict__)
  for n,arg in enumerate(sys.argv):
    if (fn := globals().get(f"eg{arg.replace('-', '_')}")):
      random.seed(the.seed); fn()
