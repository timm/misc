# vim: set et sts=2 sw=2 ts=2 : 
"""
tiny: XAI, MOO, stability, incremental

USAGE:
  python3 tiny.py -f csvFile

OPTIONS:
  -b --b      asdas         = 2
  -m --m      asdas         = 1
  -h --help   show help     = False
  -f --file   data file     = ../data/auto93.csv
  -s --seed   random seed   = 1234567891
"""
import sys,random
from boot import nice,settings,coerce,csv
the = settings(__doc__)

#--------------------------------------------------------------------------
def nump(x): return type(x)==list

def col(s): return list() if s[0].isupper() else dict()

def add(col,x):
  if nump(col): col += [x] 
  else: col[x] = col.get(x,0) + 1

def norm(col,x):
  if not nump(col) and x != "?": return x
  return (x - col[0]) / (col[-1] - col[0] + 1E-30)
#--------------------------------------------------------------------------
class data(nice):
  def __init__(i)   : i.cols, i.rows = None, []
  def read(i, src)  : [i.add(a)  for a  in csv(src)]; return i.ok()
  def adds(i, a=[]) : [i.add(a1) for a1 in a];        return i.ok()
  def ok(i)         : [col.sort() for col in i.cols.all if nump(col)]
  def add(i,a):
    if i.cols: 
      [add(col,x) for col,x in zip(i.cols.all,a) if x != "?"]
      i.rows += [a]
    else:
      i.cols = {all=[], x={}, y={} , names=a}
      for n,s in enumerate(a):
        i.cols.all += [col(s)]
        if s[-1] != "X":
          (i.cols.y if s[0].isupper() else i.cols.x)[n] = i.cols.all[-1]
  def d2h(i,a):
    d,n=0,0
    for c,(col,name) in enumerate(zip(i.cols.y, i.cols.names)):
      heaven = 0 if name[-1]=="-" else 1
      n += 1
      d  = d + (heaven - norm(col, a[c])^2
    return (d/n) ^ (1/2)
  def sorts(i, rows):
    return sorted(rows, key=lambda r: i.d2h(r))
  def bestRest(i,rows):
    a   = i.sorted(rows)
    mid = int(a//2)
    if len(a) <=  20: return {0:a[:mid], 1:a[mid:]}
    j = 2 if len(a) <= 100 else 1
    n = j*len(a)**2
    return {0:random.sample(a[:-n], k=2*n),  1:a[-n:]}
  def count(i,rows): 
    f = {}
    for klass,rows in i.bestRest(rows).items():
      for row in rows
        for cols in [i.cols.x, i.cols.y]:
          for c,col in cols.items():
            x = row[c]
            if x != "?":
              b = bins(col,x)
              k = (klass, c, b, b+1 if nump(col) else b)
              f[k] = f.get(k,0) + 1 
#--------------------------------------------------------------------------
def d2h(data,row):
  d,n = 0,0
  for col in data.cols.y:
    heaven = 0 
def thing(data,start=1,pause=7):
  tmp  = sorted(data.rows[start:pause],key=lambda row: d2h(data,row))

  mid  = len(tmp)//2
  half = mid//2
  count(data, tmp[:half],0)
  count(data, tmp[half:],1)
 
def count(data,lst,klass)
  {col:for n,col in data.y.items():
v
def inc(d, kl,col):
  for x in lst:
    if x not in d: 
      d[x]={}
    inc(d[x], *lst[1:]) 
    

#--------------------------------------------------------------------------
def cuts2Rule(cuts):
  """Cuts belong to columns. Cuts are divided up into those columns.
  If a column has more than one cut, that is a disjunction. Tha
  final call to `set` removes duplicates."""
  d = defaultdict(list)
  [d[cut[0]].append(cut) for cut in cuts]
  return tuple(sorted([tuple(sorted(set(x))) for x in d.values()]))

def selects(rule, labelledRows):
  "`Rule`s can select rows from multiple `labelledRows`."
  return {label: select(rule,rows) for label,rows in labelledRows}

def select(rule, rows): 
  "`Rule`s can pull specific `rows`."
  return [row for row in rows if ands(rule,row)]

def ands(rule,row):
  "`Rule` is a  collection of  conjunctions. If any are false, then the rule fails."
  for cuts in rule:
     if not ors(row[cuts[0][0]], cuts): return False
  return True

def ors(x, cuts):
  "For each disjunction in `cuts`, at least one c`cut` must be true (else, return None)."
  for cut in cuts:
    if true(x, cut): return cut

def true(x, cut):
  "Is it true that this `cut` hold `x`?"
  _,lo,hi = cut
  return  x=="?" or lo==hi==x or  x > lo and x <= hi

if __name__ == "__main__":
  the = the.cli()
  random.seed(the.seed)
  d=Data(the.file)

# class obj:       __repr__= lambda i:printd(i.__dict__, i.__class__.__name__)
# class box(dict): __repr__= lambda i:printd(i); __setattr__=dict.__setitem__; __getattr__=dict.get
#
# the=box(file="../data/auto93.csv")
#
# class SYM(obj): 
#   def __init__(i,at=0,txt=" "): 
#    i.n,i.at,i.txt,i.has=0,0," ",{}
#   def add(i,x): 
#    if x != "?": 
#      i.n += 1
#      i.has[x] = 1 + i.has.get(x,0) 
#
# class NUM(obj): 
#   def __init__(i,at=0,txt=" "): 
#     i.n,i.at,i.txt,i.lo,i.hi,i.mu = 0,0," ",1E30,-1E30,0
#     i.heaven = 0 if i.txt[-1] == "-" else 1
#   def add(i,x)   :
#     if x != "?": 
#       i.n += 1
#       i.lo = min(x,i.lo)
#       i.hi = max(x,i.hi)
#       i.mu = i.mu + (x-i.mu)/i.n
#   def d2h(i,x):
#     return abs(i.heaven - i.norm(x))
#   def norm(i,x):
#     return x if x=="?" else (x-i.lo)/(i.hi - i.lo + 1E-30)
#
# class COLS(obj):
#   def __init__(i,names):
#     i.all, i.x, i.y, i.klass  = [],[],[],None
#     for n,s in enumerate(names):
#       a,z  = s[0], s[-1]       
#       col  = (NUM if a.isupper() else SYM)(at=n,txt=s)
#       i.all += [col]
#       if z != "X":
#         if z == "!": i.klass = col
#         (i.y if z in "!+-" else i.x).append(col)
#   def add(i,a):
#     [col.add(a[col.at] ) for cols in [i.x, i.y] for col in cols]
#
# class DATA(obj):
#   def __init__(i,src): 
#     i.rows=[]; i.cols=None
#     [i.add(a) for a in csv(src)]
#   def add(i,a):
#     if i.cols: i.cols.add(a); i.rows += [a]
#     else: i.cols = COLS(a)
#   def d2h(i,t):
#     n,d=0,0
#     for col in i.cols.y:
#       n+=1
#       d+= col.d2h(t[col.at])^2
#     return (d/n) ^ (1/2)
#   def sort(i, rows):
#     return sorted(rows, key=lambda row: i.d2h(row))
#
# def printd(d,pre=""): return pre + str(d)
#
# def coerce(s):
#   try: return ast.literal_eval(s)
#   except Exception: return s.strip()
#
# def csv(file="-"):
#   with  fileinput.FileInput(file) as src:
#     for line in src:
#       line = re.sub(r'([\n\t\r"\' ]|#.*)', '', line)
#       if line: yield [coerce(x) for x in line.split(",")]
#
# print(DATA(the.file).cols.x[-1])
