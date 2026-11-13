#!/usr/bin/env python3
# vim: set ts=2 sw=2 sts=2 et cc=75 :
"""a.py: simulated annealing -- downhill always, uphill sometimes.

usage: python3 a.py [-h] [name=value ...]

flags:
  fun=f_bowl       what to minimize: any f_* defined below
  x=10.0           where to start
  t=100.0          starting temperature
  cool=0.99        what `t` is multiplied by each step
  n=1000           steps
  seed=1234567891  random number seed
"""
import math, random, re, sys

class o:
  def __init__(i, **d): i.__dict__.update(d)
  def __repr__(i): return str(i.__dict__)

the = o(fun="f_bowl", x=10.0, t=100.0, cool=0.99, n=1000, seed=1234567891)

def f_bowl(x): return (x - 3) ** 2 + 4
def f_ripple(x): return (x - 3) ** 2 / 8 + 3 * math.sin(3 * x) + 4
def f_cliff(x): return 4 + abs(x-3) - 6 * math.exp(-((x - 3) ** 2) / .05)

def value(s):
  try: return int(s)
  except ValueError:
    try: return float(s)
    except ValueError: return s

def sa(f, x):
  random.seed(the.seed)         # at the start, so a run is repeatable
  t, c = the.t, f(x)
  bx, bc = x, c                 # the best SEEN: `x` may wander off it
  for _ in range(the.n):
    nx = x + random.uniform(-1, 1)
    nc = f(nx)
    d  = nc - c
    if d < 0 or random.random() < math.exp(-d / t):
      x, c = nx, nc
      if c < bc: bx, bc = x, c
    t *= the.cool
  return bx, bc

def main(argv):
  "Read `name=value` into `the`, then run the demo."
  for s in argv:
    if s in ("-h", "--help"): return print(__doc__)
    if m := re.match(r"^(\w+)=(.*)$", s): setattr(the, m[1], value(m[2]))
    else: sys.exit("? " + s)
  f = globals().get(the.fun) if str(the.fun).startswith("f_") else None
  if not callable(f):
    sys.exit(f"? fun={the.fun}; want one of: " +
             " ".join(sorted(k for k in globals() if k.startswith("f_"))))
  bx, bc = sa(f, the.x)
  print(f"{the.fun:9} x={bx:8.4f} cost={bc:9.4f}")

if __name__ == "__main__": main(sys.argv[1:])
