# vim: set et sts=2 sw=2 ts=2 : 
"""
tiny: XAI, MOO, stability, incremental

USAGE:
  python3 tiny.py -f csvFile

OPTIONS:
  -b --b    asdas     = 2
  -m --m    asdas     = 1
  -h --help show help = False
  -f --file data file = ../data/auto93.csv
"""
import ast, yaml,json, re,ast,sys,random,fileinput

def coerce(s):
  "coerce to int,float or bool"
  try:              return ast.literal_eval(s)
  except Exception: return s.strip()

def nicely(x,short=True): 
  "convert `x` to a nested dictionary; pretty print that dictionary"
  return yaml.dump(json.loads(json.dumps(x, default=lambda i: i.__dict__)),
                   default_flow_style=True)

class Nice:
  "`nice` objects can represent themselves nicely"
  __repr__= lambda i: nicely(i)

class Settings(Nice):
  def __init__(i,s):
    "Settings are parsed from __doc__"
    goal = r"\n\s*-\w+\s*--(\w+).*=\s*(\S+)"
    i.__dict__.update(**{m[1]:coerce(m[2]) for m in re.finditer(goal,s)})
  def cli(i):
    """Settings can be updated from command line. Boolean settings need no
    argument (we just flip the default). if we see -h, print help and exit"""
    for k, v in i.__dict__.items():
      s = str(v)
      for j, x in enumerate(sys.argv):
        if ("-"+k[0]) == x or ("--"+k) == x:
          s = "True" if s == "False" else ("False" if s == "True" else sys.argv[j+1])
        i.__dict__[k] = coerce(s)
    if i.help: sys.exit(print(__doc__))
    return i

the = Settings(__doc__)
print(the)
#--------------------------------------------------------------------------
def nump(x): return type(x)=="list"
def col(s) : return [] if s[0].isupper() else {}
def add(col,x):
  if x != "?": 
    if nump(col): col += [x]
    else        : col[x] = col.get(x,0) + 1

class Data(Nice):
  def __init__(i): i.cols=None; i.rows=[]
  def read(f): [i.add(a) for a in csv(f)]
  def add(i,a):
    if i.cols: i.rows += [i.cols.add(a)]
    else: i.cols = COLS(a)

def csv(file="-"):
  with  fileinput.FileInput(file) as src:
    for line in src:
      line = re.sub(r'([\n\t\r"\' ]|#.*)', '', line)
      if line: yield [coerce(x) for x in line.split(",")]

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
