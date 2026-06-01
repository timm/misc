#!/usr/bin/env python3 -B
from types import SimpleNamespace as o
import random, math, sys, re
help=re.findall(r"(\w+)=(\S+)","""
fft.py, fastmap bi-cluster + jaccard overlap of two runs
(c) 2025, Tim Menzies <timm@ieee.org>, MIT license

Options:
 -s --random seed   seed=1234567891
 -t number of trees trees=20
 -f data file       file=auto93.csv
""")

BIG = math.inf

def coerce(z):
  for f in (int, float):
    try: return f(z)
    except: pass
  z = z.strip(); return {'True':True,'False':False}.get(z, z)

the= o(**{k:coerce(v) for k,v in help})

def Num(at=0,txt=""): return o(at=at,txt=txt,lo=BIG,hi=-BIG,mu=0,n=0,heaven=0 if txt[-1:]=="-" else 1)
def Sym(at=0,txt=""): return o(at=at,txt=txt,has={})
def Data()          : return o(rows=[], cols=[])
def isSym(c):  return "has"  in c.__dict__
def isData(c): return "rows" in c.__dict__
def add(it, v):
  if v == "?": return v
  if isSym(it): it.has[v] = 1 + it.has.get(v, 0)
  elif isData(it):
    if it.cols: it.rows.append([add(c, v[c.at]) for c in it.cols.all])
    else: it.cols = dataHeader(v)
  else:
    it.n += 1; d = v - it.mu; it.mu += d / it.n
    it.lo = min(it.lo, v); it.hi = max(it.hi, v)
  return v

def adds(src, it=None):
  for x in iter(src): add((it := it or Num()), x)
  return it

def dataHeader(names):
  cols = o(all=[], x=[], y=[])
  for c, s in enumerate(names):
    col = (Num if s[0].isupper() else Sym)(c, s)
    cols.all.append(col)
    if s[-1] == "X": continue
    (cols.y if s[-1] in "!-+" else cols.x).append(col)
  return cols

def dataRead(file):
  data = Data()
  for line in open(file):
    line = line.strip()
    if line and line[0] != "#":
      add(data, [coerce(x) for x in line.split(",")])
  return data
def norm(c, v): return (v - c.lo) / (c.hi - c.lo + 1E-32)

def disty(data, r, p=2):
  d, n = 0, 0
  for c in data.cols.y:
    if isSym(c) or r[c.at]=="?": continue
    n += 1; d += abs(norm(c, r[c.at]) - c.heaven)**p
  return (d/n)**(1/p) if n else 0

def distx(data, r1, r2, p=2):
  d, n = 0, 0
  for c in data.cols.x:
    n += 1; v1, v2 = r1[c.at], r2[c.at]
    if v1 == "?" and v2 == "?": d += 1; continue
    if isSym(c): d += (0 if v1 == v2 else 1)**p
    else:
      v1 = norm(c,v1) if v1 != "?" else (0 if norm(c,v2) > .5 else 1)
      v2 = norm(c,v2) if v2 != "?" else (0 if v1 > .5 else 1)
      d += abs(v1 - v2)**p
  return (d/n)**(1/p)

def fastmap(data):
  stop, ind, nxt = int(math.sqrt(len(data.rows))), {}, [0]
  def _go(rows):
    if len(rows) <= stop:
      cid = nxt[0]; nxt[0] += 1
      for r in rows: ind[id(r)] = cid
      return
    far = lambda ref: sorted(rows, key=lambda r: distx(data, ref, r))[int(0.99*len(rows))]
    a = random.choice(rows); b = far(a); a = far(b)
    c = distx(data, a, b) + 1E-32
    proj = lambda r:(distx(data,r,a)**2 + c*c - distx(data,r,b)**2)/(2*c)
    rows = sorted(rows, key=proj); m = len(rows)//2
    _go(rows[:m]); _go(rows[m:])
  _go(data.rows)
  return ind

def jaccards(data, N=None, frac=0.1):
  N = N or the.trees
  dy = {id(r): disty(data, r) for r in data.rows}
  def clusters(ind):
    c = {}
    for r in data.rows: c.setdefault(ind[id(r)], set()).add(id(r))
    ranked = sorted(c.values(), key=lambda s: sum(dy[r] for r in s)/len(s))
    return ranked[:max(1, int(len(ranked)*frac))]
  parts = [clusters(fastmap(data)) for _ in range(N)]
  jac = lambda a,b: len(a & b)/len(a | b)
  out = []
  for i, p in enumerate(parts):
    for s in p:
      bests = [max(jac(s,t) for t in parts[j]) for j in range(N) if j!=i]
      out.append(sum(bests)/len(bests))
  return sorted(out)

def eg_h(): print(__doc__)
def eg__jaccards():
  for j in jaccards(dataRead(the.file)): print(int(100*j))
if __name__ == "__main__":
  for n, arg in enumerate(sys.argv):
    for k in the.__dict__:
      if arg == "-" + k[0]: the.__dict__[k] = coerce(sys.argv[n+1])
    if (fn := globals().get(f"eg{arg.replace('-','_')}")):
      random.seed(the.seed); fn()
