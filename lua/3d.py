#!/usr/bin/env python3
import random, sys

BIG, the = 1E32, {"bins": 10, "seed": 10, "p": 2, "file": "../../moot/optimize/misc/auto93.csv"}
random.seed(the["seed"])

class DATA:
  def __init__(i,names):
    i.lo, i.hi, i._rows = {}, {}, []
    i.names, i.x, i.y = names, {}, {}
    for c, s in enumerate(names):
      if not s[-1] == "X":
        if s[0].isupper(): i.lo[c], i.hi[c] = BIG, -BIG
        (i.y if s[-1] in "!+-" else i.x)[c] = c
        if s[-1] == "-": i.y[c] = 0
        if s[-1] == "+": i.y[c] = 1
 
  def norm(i,c,x): 
    return x if x == "?" else (x - i.lo[c]) / (i.hi[c] - i.lo[c] + 1/BIG)

  def dist(i,a,b):
    def _dist(c,x,y):
      if x == "?" and y == "?": return 1
      if c not in i.lo: return 0 if x == y else 1
      x = i.norm(c,x) if x != "?" else (0 if y > 0.5 else 1)
      y = i.norm(c,y) if y != "?" else (0 if x > 0.5 else 1)
      return abs(x - y)
    d = sum(_dist(c, a[c], b[c]) ** the["p"] for c in i.x)
    return (d / len(i.x)) ** (1 / the["p"])

  def add(i,row):
    i._rows.append(row)
    for c,x in enumerate(row):
      if x != "?" and c in i.lo:
        i.lo[c] = min(i.lo[c], x)
        i.hi[c] = max(i.hi[c], x)
  
def cat(x):
    if isinstance(x, list): return "{" + ", ".join(map(cat, x)) + "}"
    if isinstance(x, dict): return str({k:v for k,v in x.items() if str(k)[0] != "_"})
    if hasattr(x,"__dict__"): return x.__class__.__name__ + cat(x.__dict__)
    if isinstance(x, float) and x != int(x): return f"{x:.3g}"
    return str(x)

def atom(x):
  for cast in (int, float):
    try: return cast(x)
    except: pass
  x = x.strip()
  return (x.lower() == 'true') if x.lower() in ("true", "false") else x

def atoms(row): return [atom(x) for x in row]

def read_csv(path):
  with open(path) as f:
    for line in f:
      yield [atom(x) for x in line.strip().split(",")]

def main(file):
  d = None
  for row in read_csv(file or the.file):
    if not d: d = DATA(atoms(row))
    else: d.add(atoms(row))
  return d

if __name__ == "__main__":
  d = main(sys.argv[1] if len(sys.argv) > 1 else the["file"])
  print(cat(d))

