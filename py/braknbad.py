#!/usr/bin/env python3 
# vim: ft=python:nospell:sta:et:sw=2:ts=2:sts=2
""" 

## Breaking bad

"I am the one who knocks."

      .-------.
      |       |
    -=_________=-
      ___   ___
     |___)=(___|
          |
         ###
        #####
        # = #
         ###

- Divide data into regions of bad and better.
- Find the least you need to do to nudge things away from bad.

```
Better
.------.
|    36| <--- a score you want to minimize
| Br   |
.______.______.
       |    56| <--- a larger, hence worse, score
       | Ba   |
       .______.
        Bad
```

"""

def config(): 
  """Set global options.i to change these, at the comamnd-line,
   type
       
        ./braknbad GROUP -OPTION value

  where GROUP is one of char,dist,do,etc and OPTION
  is some sub-setting in GROUP; e.g. 

        ./braknbad dist -p 3

  """ 
  return o(
   char = o( no    = "?",
             less  = "<",  
             more  = ">",
             sep   = ",", 
             doomed= r'([\n\t\r ]|#.*)'),
   dist = o( p=2 ),
   do   = o(  run=r"."     ),
   nb   = o(  k=1, m=2     ),
   row  = o(  doms = 64    ),
   two  = o(  better = 0.2 ),
   div  = o(  cohen  = 0.3,
              trivial= 1.05,
              min    = 0.6)
   )


#--------- --------- --------- --------- -------- ----------
import re,sys,traceback,random
from copy import deepcopy as kopy

class o:
  """A few of my favorite things; (1) fast creation
  of instance attributes; (2) hash id management; 
  (3) pretty print of instances."""
  id = 0
  "used to generate ids for an instance"
  def __init__(i,**d) : i.__dict__.update(**d)
  def identity(i): 
    "assign a new unique integer as 'id' of this instance."
    i.id = o.id = o.id+ 1;return i.id
  def __hash__(i):
    "Allow this instance to be used in sets or dictionary keys."
    return i.id
  def __repr__(i):
    """Print attributes in alphabetical order, skipping
     'private' attributes (those starting with '_')."""
    lst= [(k, v) for k,v in i.__dict__.items() if k[0] != "_"]
    return i.__class__.__name__ + '{' + ", ".join(
           [('%s=%s' % (k, v)) for k,v in sorted(lst)]) +'}'

any = random.choice
p=lambda z: int(round(z*100,0))

def same(x):
  "The identity function. Returns 'x'."
  return x

def atom(x):
  "Coerce the string x into the right kind of atom."
  try: return int(x)
  except:
    try: return float(x)
    except: return x


def cli(d,args=sys.argv[1:]):
  """Allow command lines args to update fields in the THE object.
     Example usage: THE = cli(THE)."""
  args   = [atom(x) for x in args]
  what   = {}
  groups = d.__dict__
  while args:
    arg = args.pop(0)
    if arg in groups:
      what = groups[arg].__dict__
    else:
      assert isinstance(arg,str) and arg[0] == "-", "bad flag '%s'" %arg
      arg = arg[1:]
      assert arg in what, "%s not one of %s" % (arg,list(what.keys()))
      old = what[arg]
      if isinstance(old, bool):
        what[arg] = not what[arg]
      else:
        val = args.pop(0)
        assert type(old)==type(val), "'%s' value not of type '%s'"%(
                                       arg,type(old))
        what[arg] = val
  return d   

THE=config()

class Eg(o):
  "Manage a list of demo/test functions."
  all = []  # asdas"
  "Place to store demos/tests."
  n   = 0
  "Number of called demos/tests."
  tell= "#Tests 0 tries 0 fails 100% passes."
  "Report string (summarizes the run)."
  y   = 0
  "Number of demos/tests with assert failers"
  def run(): 
    "Run the list of demos/tests."
    [Eg.run1(one) for one in Eg.all]
  def run1(f):
    "Run a demo/test. If it fails, update the Eg.no counter."
    if re.match(THE.do.run, f.__name__):
      print("\n-----| %s |%s" % (f.__name__,"-"*40))
      if f.__doc__:
        print("# "+ re.sub(r'\n[ \t]*',"\n# ", f.__doc__))
      Eg.y += 1
      try:    
        f()
      except: 
        Eg.n += 1
        y,n  = Eg.y, Eg.n
        print(traceback.format_exc())
        Eg.tell = "#Tests %s tries %s fails %s%% passed." % (
                    y,n, round(100*(y/(y+n+0.0001)),0))

def eg(f): 
  """Convenience function. Decorator that adds 
     functions to the list managed by the `Eg` class."""
  Eg.all += [f]; return f

class Row(o):
  "Holder for lists of values and their discretized ranges."
  def __init__(i,lst):
    i.cells=lst
    i.ranges=[None]*len(lst)
    i.score=0
  def scoring(i,j,t):
    "Check if this row 'i' is better than row 'j'."
    n = len(t.cols.objs)
    s1 = s2 = 0
    for c in t.cols.objs:
      x,y = i.cells[c.pos], j.cells[c.pos]
      x,y = c.norm(x), c.norm(y)
      s1 -= 10**( c.w * (x-y)/n )
      s2 -= 10**( c.w * (y-x)/n )
    return s1/n < s2/n

class Tbl(o):
  """Manage a list of Rows. Keep statistics on each column
   in `objs` or `decs` objects (which are either `Num` or
  `Sym` instances)."""
  def __init__(i,names=[], rows=None):
    i.rows = []
    i.cols = o(all=[], decs = [], objs=[], numdecs=[])
    if names: 
      i.header(names)
      i.names = names
    if rows:  
      i.read(rows)
      i.names = [ c.txt for c in i.cols.all ]
  def centroid(i):
    "Return the middle."
    return [ c.centroid() for c in i.cols.all ]
  def scoring(i):
    """Score each row (using the row's `scoring` method).
     If there are too many rows to score fully, then
     just score against a random number."""
    n = THE.row.doms
    for r1 in i.rows:
      if n < len(i.rows):
        tmp= sum(r1.scoring(any(i.rows),i) for _ in range(n)) 
      else: 
        tmp= sum(r1.scoring(r2,i) for r2 in i.rows)
      r1.score = tmp/n
  def cook(i):
    i.scoring()
    r=[]
    for col in i.cols.numdecs:
       d=  Div2(i.rows,
                   x=lambda row: row.cells[col.pos],
                   y=lambda row: row.score)
       r+= d.ranges
    return d.b4, sorted(r,key=lambda z:z.stats.mu)
  def clone(i):
    """Return an (empty) table that can read rows like 
    those seen in this table."""
    return Tbl(i.names)
  def header(i,names):
    "Convert a list of names into `Num`s and `Sym`s as appropriate."
    for n,s in enumerate(names):
      w     = -1 if s[0] == '<' else 1
      x     = (Num    if s[0] in "<>$" else Sym)(s,n,w)
      what  = i.cols.objs if s[0] in '<>' else i.cols.decs
      what += [x]
      i.cols.all += [x]
      if s[0] == "$": i.cols.numdecs += [x]
  def read(i,src):
    """Read rows from some src. Add to this table.
      If this table does not know what is columns are yet,
      then read those from the first row."""
    for row in src:
      i + row if i.cols.all else i.header(row)
  def __add__(i,lst):
    "Add a new row.  Update the column statistics."
    lst     = [ c + lst[c.pos] for c in i.cols.all ]
    i.rows += [ Row(lst) ]
  def like(i,lst,ns):
    "Report the likelihood that `lst` belongs in this table."
    n    = len(i.rows)
    k, m = THE.nb.k, THE.nb.m
    like = prior = (n + k) / (ns + k*2)
    for c in i.all.decs:
      x     = lst[c.pos]
      if x == THE.char.no: continue
      f     = c.bag.get(x,0)
      inc   = (f + m*prior) / (n + m)
      like *= inc
    return like

class Thing(o):
   def xpect(i,j):
    n = i.n + j.n
    return i.n/n * i.variety() + j.n/n * j.variety()

class Num(Thing):
  def __init__(i,txt="",pos=0,w=1,key=same,inits=[]):
    i.id = i.identity()
    i.txt,i.pos = txt,pos
    i.w=w
    i.n,i.mu,i.m2 = 0,0,0
    i.lo,i.hi     = 10**32, -10**32
    i.key = key
    [i + one for one in inits]
  def variety(i) : return i.sd
  def centroid(i): return i.mu
  def norm(i,x): 
    return (x - i.lo) / (i.hi - i.lo + 10**-32)
  def __add__(i,x):
    x = i.key(x)
    if x == THE.char.no: return x
    x = float(x)
    if x < i.lo: i.lo = x
    if x > i.hi: i.hi = x
    i.n  += 1
    d     = x - i.mu
    i.mu += d/i.n
    i.m2 += d*(x - i.mu)
    i.sd  = i.sd0()
    return x
  def __sub__(i, x):
    if i.n < 2:
      i.n, i.mu, i.m2 = 0, 0, 0
    else:
      i.n -= 1
      x = i.key(x)
      d = x - i.mu
      i.mu -= d / i.n
      i.m2 -= d * (x - i.mu)
      i.sd = i.sd0()
    return x
  def sd0(i):
    if i.m2 < 0: return 0
    if i.n  < 2: return 0
    return (i.m2/(i.n - 1 + 10**-32))**0.5

class Sym(Thing):
  def __init__(i,txt="",pos=0,w=1, key=same, inits=[]):
    i.id = i.identity()
    i.txt,i.pos = txt,pos
    i.w=w
    i.n,i.most,i.mode,i.bag = 0,0,None,{}
    i.key=key
    i._ent=None
    [i + one for one in inits]
  def variety(i): return i.ent()
  def centroid(i): return i.mode
  def __add__(i,x):
    x = i.key(x)
    if x == THE.char.no: return x
    i._ent= None
    i.n += 1
    c = i.bag[x] = i.bag.get(x,0) + 1
    if c > i.most:
       i.most, i.mode = c, x
    return x
  def ent(i):
    if not i._ent:
      i._ent = 0
      for _,v in i.bag.items():
        p =  v/i.n
        i._ent -= p*math.log(p,2)
    return i._ent

class Two(o):
  """Stores two tables: one for `_bad` things
     and one for `_better` things."""
  def __init__(i,t, lst):
    i.bad, i.better = t.clone(), t.clone()
    lst = sorted(lst, key=lambda z:z.count)
    n   = int(len(lst)*THE.two.better)
    for m,one in enumerate(lst):
      a = one.cells
      (i.bad + a) if m < n else (i.better + a)
  def p(i,lst): 
    ns = len(i.bad.rows) + len(i.better.rows)
    l1 = i.bad.like(   i,lst, ns)
    l2 = i.better.like(i,lst, ns)
    return l1/(l1+l2), l2/(l1+l2)

def first(lst): return lst[0]
def last(lst) : return lst[-1]

class Div2(o):
  """
  Recursively divide a list of numns by finding splits
  that minimizing the expected value of the standard
  deviation (after the splits).
  """
  def __init__(i,lst, x=first, xis=Num, y=last, yis=Num):
    i.x, i.y   = x,y
    i.xis      = lambda lst=[]: xis(inits=lst, key=i.x) # a collector for x things
    i.yis      = lambda lst=[]: yis(inits=lst, key=i.y) # a collector for y things
    i._lst     = sorted([one for one in lst if x(one) is not THE.char.no], key=x)
    i.xs       = i.xis(i._lst)
    i.ys       = i.yis(i._lst)
    i.b4       = kopy(i.ys)
    i.step     = int(len(i._lst)**THE.div.min) # each split need >= 'step' items
    i.stop     = x(last(i._lst))               # top list value
    i.start    = x(first(i._lst))              # bottom list value
    i.ranges   = []                            # the generted ranges
    i.epsilon  = i.xs.sd * THE.div.cohen     # bins must be seperated >= epsilon
    i.__divide(1, len(i._lst), i.xs, i.ys, 1)

  def __divide(i, lo, hi, xr, yr, rank):
    """Find a split between lo and hi, then recurse on each split.
     If no split can be found then assign everything a rank of 'rank'.
     'xr' and 'yr' are statistics on the whole x,y space from lo to hi."""
    xb4       = kopy(xr)
    xb4.stats = kopy(yr)
    xl        = i.xis()
    yl        = i.yis()
    best      = yr.variety()
    cut       = None
    for j in range(lo,hi):
      xl + i._lst[j]
      yl + i._lst[j]
      xr - i._lst[j]
      yr - i._lst[j]
      if xl.n >= i.step:
        if xr.n >= i.step:
          now   = i.x( i._lst[j]   )
          after = i.x( i._lst[j+1] )
          if now == after: continue
          if abs(xr.mu - xl.mu) >= i.epsilon:
            if after - i.start >= i.epsilon:
              if i.stop - now >= i.epsilon:
                xpect = yl.xpect(yr)
                if xpect*THE.div.trivial < best:
                  best, cut = xpect, j+1
    if cut:
      ls   = i._lst[lo:cut]
      rs   = i._lst[cut:hi]
      rank = i.__divide(lo, cut, i.xis(ls), i.yis(ls), rank) + 1
      rank = i.__divide(cut, hi, i.xis(rs), i.yis(rs), rank)
    else:
      xb4.rank  = rank
      xb4.uses = set(i._lst[lo:hi])
      i.ranges += [ xb4 ]
      for row in i._lst[lo:hi]:
        row.ranges[xb4.pos] = xb4
    return rank

def csv(file):
  use, txt = [], ""
  with open(file) as fs:
    for line in fs:
      txt += re.sub(THE.char.doomed, '', line)
      if txt and txt[-1] != THE.char.sep:
        lst = txt.split(THE.char.sep) 
        if lst:
          txt = ""
          use = use or [n for n,s in enumerate(lst) 
                       if s[0] != THE.char.no ]
          if len(lst) != len(use):
            err("wanted %s cells, got %s in %s", 
                (len(use), len(lst),lst))
          yield [lst[n] for n in use]

@eg
def doco():
  import subprocess
  subprocess.call(["/usr/bin/pydoc","./braknbad"])

if __name__ == "__main__":
  THE = cli(THE)
  #Eg.run()
  t= Tbl(rows = csv("../data/auto93.csv"))
  b4, after = t.cook()
  print(p(b4.mu), p(b4.sd),b4.n)
  for r in after:
    print(p(r.stats.mu), p(r.stats.sd),r.n)

