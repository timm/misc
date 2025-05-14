#!/usr/bin/env python3
import random, sys
any = random.choice
many= random.choices

class o(dict):
    def __getattr__(i, k)   : return i[k]
    def __setattr__(i, k, v): i[k] = v

BIG, the = 1E32, o(bins= 10, seed= 10, p= 2, 
                   some=30, poles=4,
                   file= "../../moot/optimize/misc/auto93.csv")
random.seed(the.seed)

class DATA:
  def __init__(i,src):
    src = iter(src)
    i.names, i.nums, i.x,i.y, i._rows = next(src),{},{},{}, []
    for c, s in enumerate(i.names):
      if not s[-1] == "X":
        if s[0].isupper(): i.nums[c] = o(lo=BIG, hi=-BIG)
        if s[-1] == "-": i.y[c] = 0
        elif s[-1] == "+": i.y[c] = 1
        else: i.x[c]=1
    [i.add(row) for row in src]
 
  def add(i,row):
    i._rows += [row]
    for c,x in enumerate(row):
      if x != "?" and c in i.nums:
        n = i.nums[c]
        n.lo = min(x, n.lo)
        n.hi = max(x, n.hi)
  
  def norm(i,c,x): 
    n= i.nums[c]; return  (x - n.lo) / (n.hi - n.lo + 1/BIG)

  def dist(i,a,b):
    def _dist(c,x,y):
      if x == "?" and y == "?": return 1
      if c in i.nums: 
        x = i.norm(c,x) if x != "?" else (0 if y > 0.5 else 1)
        y = i.norm(c,y) if y != "?" else (0 if x > 0.5 else 1)
        return abs(x - y)
      return x != y
    lst= [ _dist(c, a[c], b[c]) ** the.p for c in i.x] 
    return (sum(lst) / len(lst)) ** (1 / the.p)

  def poles(i):
    r0, *some = many(i._rows, k=the.some+1)
    out = [max(some, key = lambda r1: i.dist(r1,r0))]
    for _ in range(the.poles-1):
      out += [max(some, key=lambda r2: sum(i.dist(r2,r1) for r1 in out))]
    return out

#----------------------------------------------------------------------------------------
def cat(x,   isa=isinstance):
  if isa(x, list)          : return "{" + ", ".join(map(cat, x)) + "}"
  if isa(x, (float,int))   : return str(x) if x ==int(x) else f"{x:.3g}"
  if isa(x, dict): return cat([f":{k} {cat(v)}" for k,v in x.items() if str(k)[0]!="_"])
  if hasattr(x,"__dict__") : return x.__class__.__name__ + cat(x.__dict__)
  return str(x)

def cast(x):
  for what in (int, float):
    try: return what(x)
    except: pass
  x = x.strip()
  y = x.lower()
  return (y== 'true') if y in ("true", "false") else x

def csv(path):
  with open(path) as f:
    for line in f:
      yield [cast(x) for x in line.strip().split(",")]

#---------------------------------------------------------------------------------------/
if __name__ == "__main__":
  f = sys.argv[1] if len(sys.argv) > 1 else the.file
  d = DATA(csv(f))
  print(cat(d))
  [print(r) for r in d.poles()]

