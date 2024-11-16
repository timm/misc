import ast
import sys

class o:
  def __init__(i,**d): i.__dict__.update(d)
  def __repr__(i): return '('+' '.join(f":{k} {v}" for k,v in i.__dict__.items())+')'

the = o(train="../../moot/optimize/misc/auto93.csv")

def coerce(s):
  try: return ast.literal_eval(s)
  except: return s.strip()

def csv(f):
  with open(f, "r") as file:
    for line in file.readlines():
      yield [coerce(s) for s in line.split(",")]

def Data(src):
  rows, cols = [], None
  for row in src:
    if cols: rows += [[add(col,x) for col,x in zip(cols.all,row) if x != "?"]]
    else   : cols=Cols(row)
  return  o(rows=rows, cols=cols)

def Cols(names):
  all,x,y = [],[],[]
  for at,s in enumerate(names):
    all += [o(mu=0,sd=0,all=[]) if s[0].isupper() else {}]
    (y if s[-1] in "+-!" else x).append(all[-1])
  return o(names=names, all=all, x=x, y=y)
    
def add(col,x):
  if isinstance(col,dict): col[x] = 1 + col.get(x,0)
  else: col.all += [x]
  return x

class eg:
  def the(_): print(the)
  def load(i): 
    d= Data(csv(sys.argv[i]))

def sd(num,lst):
  num.mu = sum(lst)/len(lst)
  num.sd = (sum((x-num.mu)**.5 for x in lst) / (len(lst) - 1))**.5
  return num

for j,s in enumerate(sys.argv):
  getattr(eg,s[2:], lambda _:_)(j+1)
