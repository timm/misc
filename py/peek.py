#!/usr/bin/env python3 -B
from __future__ import annotations
import os, re, random, sys, bisect, math
from pathlib import Path
from random import random as rand, choices, choice, sample, shuffle
from math import log, log2, exp, sqrt, pi

class obj(dict):
  __getattr__, __setattr__ = dict.__getitem__, dict.__setitem__
  __repr__ = lambda i : o(i)

the = obj(decs=2, seed=1, p=2, few=128,
          file= Path.home() / "gits/moot/optimize/misc/auto93.csv")

#------------------------------------------------------------------------------
def Num(txt: str = "", at: int = 0):
  return obj(it=Num, txt=txt, at=at, n=0, mu=0, m2=0, heaven=txt[-1:] != "-")

def Sym(txt: str = "", at: int = 0): 
  return obj(it=Sym, txt=txt, at=at, has={})

def Col(txt: str = "", at: int = 0): 
  return (Num if txt[0].isupper() else Sym)(txt, at) 

def Cols(names: list[str]):
  all = [Col(txt, j) for j, txt in enumerate(names)]
  return obj(it=Cols, names=names, all=all,
             klass = next((c for c in all if c.txt[-1] == "!"), None),
             xs    = [c for c in all if c.txt[-1] not in "+-!X"],
             ys    = [c for c in all if c.txt[-1]     in "+-!"])

def Data(src=None):
  src = iter(src or {})
  return adds(src, obj(it=Data, rows=[], cols=Cols(next(src))))

def clone(d,rows=None):
  return Data([d.cols.names] + (rows or []))
#------------------------------------------------------------------------------
def adds(src,i=None):
  i = i or Num(); [add(i,v) for v in src]; return i

def add(i: Data|Col, v: Any) -> Any:
  if v != "?":
    if   i.it is Sym: i.has[v] = 1 + i.has.get(v, 0)
    elif i.it is Data:
      [add(c, v[c.at]) for c in i.cols.all] 
      i.rows += [v]
    elif i.it is Num:
      i.n  += 1
      delta = v - i.mu
      i.mu += delta / i.n
      i.m2 += delta * (v - i.mu)
      i.sd  = 0 if i.n < 2 else sqrt(max(0, i.m2) / (i.n - 1))
  return v

def norm(i, v):
  if v=="?": return v
  z = (v - i.mu) / (i.sd + 1e-32)
  return 1 / (1 + exp(-1.7*max(-3, min(3,z))))

def minkowski(items: Iterable[Qty]) -> float:
  tot, n = 0, 1e-32
  for item in items: tot, n = tot + item**the.p, n + 1
  return (tot/n) ** (1/the.p)

def disty(d: Data, r: Row) -> float:
  return minkowski(abs(norm(c, r[c.at]) - c.heaven) for c in d.cols.ys)

def distx(d: Data, r1: Row, r2: Row) -> float:
  return minkowski(aha(c, r1[c.at], r2[c.at]) for c in d.cols.xs)

def aha(i: Col, u: Any, v: Any) -> float:
  if u == v == "?": return 1
  if i.it is Sym: return u != v
  u, v = norm(i, u), norm(i, v)
  u = u if u != "?" else (0 if v > 0.5 else 1)
  v = v if v != "?" else (0 if u > 0.5 else 1)
  return abs(u - v)

def run(oracle,rows, K=5, budget=30):
  def Y(r):     return disty(oracle, r)
  def score(u): return sum(distx(model,unlab[u],model.rows[i])/(i+1)
                           for i in range(K))
  random.shuffle(rows)
  unlab = rows[K:][:the.few]
  model = clone(oracle, rows[:K])
  model.rows.sort(key=Y)
  for j in range(budget-K):
    if not unlab: break
    add(model, unlab.pop(min(range(len(unlab)), key=score)))
    model.rows = sorted(model.rows, key=Y)[:budget]
  return model
   
def test_run(file:str=the.file):
  d = Data(csv(file))
  Y = lambda r: disty(d, r)
  d.rows.sort(key=Y)
  lo, mid=Y(d.rows[0]), Y(d.rows[len(d.rows)//2])
  wins = lambda r: int(100*(1-(Y(r)-lo)/(mid-lo + 1e-32)))
  stats = adds(wins(run(d,d.rows).rows[0]) for _ in range(20))
  print(int(stats.mu), int(stats.sd))
      
#------------------------------------------------------------------------------
def o(it: Any) -> str:
  of = isinstance
  if callable(it): return it.__name__
  if of(it,float): return f"{it:.{the.decs}f}"
  if of(it,dict):  return "{"+ ", ".join(f"{k}={o(v)}" for k,v in it.items())+"}"
  if of(it,list):  return "{"+", ".join(map(o, it))+"}"
  return str(it)
 
def cli() -> None:
  args = [thing(x) for x in sys.argv[1:]]
  while args:
    random.seed(the.seed)
    k = re.sub(r"^-+", "", args.pop(0))
    if fn := globals().get(f"test_{k}"): 
      fn(*[args.pop(0) for _ in fn.__annotations__ if args])
    else: 
      the[k] = args.pop(0)

def thing(txt: str) -> Atom:
  txt = txt.strip()
  b = lambda s: {"true": 1, "false": 0}.get(s.lower(), s)
  for f in [int, float, b]:
    try: return f(txt)
    except ValueError: pass

def clean(s): return  s.partition("#")[0].split(",")

def csv(f: str, clean=clean):
  with open(f, encoding="utf-8") as file:
    for s in file:
      r = clean(s)
      if any(x.strip() for x in r): 
        yield [thing(x) for x in r]

#------------------------------------------------------------------------------
def test_the(): print(o(the))

def test_csv(file=the.file):
  for row in list(csv(file))[::30]: print(row)

def test_data(file=the.file):
  d = Data(csv(file))
  [print(x) for x in d.cols.xs]

#------------------------------------------------------------------------------
if __name__ == "__main__": cli()
