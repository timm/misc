#!/usr/bin/env python3 -B
import math
from types import SimpleNamespace as o

# --- Engine ---

def run(init, step, dt=1, tmax=20):
  # init format: {'key': [start, min, max]}
  u = o(**{k: v[0] for k, v in init.items()})
  out = []
  t = 0
  while t < tmax:
    v = o(**vars(u))
    step(dt, t, u, v)
    for k, item in init.items():
      lo, hi = item[1], item[2]
      setattr(v, k, max(lo, min(hi, getattr(v, k))))
    out.append((t, v))
    u = v
    t += dt
  return out

def show(data):
  if not data: return
  keys = sorted(vars(data[0][1]).keys())

  # Incremental stats
  stats = {k: o(n=0, mu=0, m2=0, sd=0) for k in keys}
  for _, row in data:
    for k in keys:
      x = getattr(row, k)
      s = stats[k]
      s.n += 1
      d = x - s.mu
      s.mu += d / s.n
      s.m2 += d * (x - s.mu)
      s.sd = (s.m2 / (s.n - 1))**0.5 if s.n > 1 else 0

  print("t", end="")
  for k in keys: print(f"{k:>8}", end="")
  print("\n ", end="")
  for k in keys: print(f"{stats[k].sd * 0.35:8.1f}", end="")
  print()

  last = {}
  for t, row in data:
    print(f"{t:2.0f}", end="")
    for k in keys:
      val = getattr(row, k)
      # Show value if first row or if change is significant (> 0.35 * SD)
      diff = abs(val - last.get(k, 0)) > stats[k].sd * 0.35
      
      if t == 0 or diff:
        print(f"{val:8.1f}", end="")
        last[k] = val
      else:
        print("       .", end="")
    print()

# --- Models ---

# Simple household diaper supply model
def diapers():
  def s(dt, t, u, v):
    sat = int(t) % 7 == 6
    v.C = u.C + dt * (u.q - u.r)
    v.D = v.D + dt * (u.r - u.s)
    v.q = 70 if sat else 0
    v.s = u.D if sat else 0
    if t == 27: v.s = 0

  return run({'C':[100,0,200], 'D':[0,0,200], 'q':[0,0,10], 
              'r':[8,0,20], 's':[0,0,100]}, s)

# Brooks, F. (1975). The Mythical Man-Month.
def brooks():
  def s(dt, t, u, v):
    comm = u.D * (u.D - 1) / 2 * 0.01
    train = u.N * 0.2
    prod = u.D * (1 - comm - train) * 10
    v.R = u.R - dt * max(0, prod)
    v.W = u.W + dt * max(0, prod)
    v.N = u.N + dt * 0.1 * u.N + (10 if t == 10 else 0)
    v.D = u.D + dt * 0.1 * u.N

  return run({'D':[20,0,100], 'N':[0,0,100], 'W':[0,0,1000], 
              'R':[1000,0,1000]}, s)

# Generic defect discovery model
def bugs():
  def s(dt, t, u, v):
    find = u.L * 0.15
    fix = u.F * 0.5
    v.L = u.L - dt * find
    v.F = u.F + dt * (find - fix)
    v.X = u.X + dt * fix

  return run({'L':[100,0,100], 'F':[0,0,100], 'X':[0,0,100]}, s)

# Cunningham, W. (1992). Technical debt.
def debt():
  def s(dt, t, u, v):
    add = u.F * 0.1
    accrue = u.D * 0.1
    repay = u.D * 0.2
    slow = 1 - u.D / 100
    v.F = u.F + dt * add * slow
    v.D = u.D + dt * (accrue - repay)
    v.V = u.V * slow

  return run({'F':[0,0,100], 'D':[0,0,100], 'V':[10,0,20]}, s)

# SIR model adapted for defect propagation
def sir():
  def s(dt, t, u, v):
    infect = u.S * u.I * 0.051
    remove = u.I * 0.15
    v.S = u.S - dt * infect
    v.I = u.I + dt * (infect - remove)
    v.R = u.R + dt * remove

  return run({'S':[90,0,100], 'I':[10,0,100], 'R':[0,0,100]}, s)

# Abdel-Hamid & Madnick (1991). Rework Cycle.
def rework():
  def s(dt, t, u, v):
    coding = u.Req * 0.2
    testing = u.Dev * 0.5
    fail = u.Test * 0.4
    passed = u.Test * 0.6
    fix = u.Rew * 0.5
    v.Req = u.Req - dt * coding
    v.Dev = u.Dev + dt * (coding - testing + fix)
    v.Test = u.Test + dt * (testing - fail - passed)
    v.Rew = u.Rew + dt * (fail - fix)
    v.Done = u.Done + dt * passed

  return run({'Req':[100,0,100], 'Dev':[0,0,100], 'Test':[0,0,100], 
              'Rew':[0,0,100], 'Done':[0,0,100]}, s)

# Generic learning/mentoring model
def learn():
  def s(dt, t, u, v):
    train = u.Jr * 0.1
    promote = u.Tr * 0.05
    mentor = u.Sr * 0.02
    v.Jr = u.Jr - dt * train + dt * mentor
    v.Tr = u.Tr + dt * train - dt * promote
    v.Sr = u.Sr + dt * promote - dt * mentor
    v.Mz = u.Mz + dt * mentor

  return run({'Jr':[20,0,100], 'Tr':[5,0,100], 'Sr':[5,0,100], 
              'Mz':[0,0,100]}, s)

# Brooks' Law extended
def brooksq():
  def s(dt, t, u, v):
    comm = u.D * (u.D - 1) / 2 * 0.0001
    train = u.N * 0.02
    prod = u.D * (1 - comm - train) * 10
    inject = prod * 0.05
    escape = u.Defects * 0.1
    v.R = u.R - dt * max(0, prod)
    v.W = u.W + dt * max(0, prod)
    v.N = u.N - dt * 0.1 * u.N + (10 if t == 10 else 0)
    v.D = u.D + dt * 0.1 * u.N
    v.Defects = u.Defects + dt * (inject - escape)
    v.Escapes = u.Escapes + dt * escape

  return run({'D':[20,0,100], 'N':[0,0,100], 'W':[0,0,1000], 
              'R':[1000,0,1000], 'Defects':[0,0,100], 
              'Escapes':[0,0,100]}, s)

# Abdel-Hamid & Madnick (1991). Defect Map.
def defmap():
  def s(dt, t, u, v):
    intro = u.PC * 0.3 - u.DE * 0.2
    detect = u.TE * u.DI * 0.4
    escape = u.DI * (1 - u.TE * 0.4)
    oper = u.RD * u.OU * 0.15
    v.DI = u.DI + dt * intro
    v.DD = u.DD + dt * detect
    v.RD = u.RD + dt * (escape - oper)
    v.OD = u.OD + dt * oper
    v.PC, v.DE, v.TE, v.OU = u.PC, u.DE, u.TE, u.OU

  return run({'PC':[20,0,100],    # problem complexity (aux)
              'DE':[20,0,100],    # design effort (aux)
              'TE':[2.5,0,10],    # testing effort (aux)
              'OU':[35,0,100],    # operational usage (aux)
              'DI':[2.43,0,100],  # defects introduced
              'DD':[0,0,100],     # defects detected
              'RD':[0,0,100],     # residual defects
              'OD':[0,0,100]},    # operational defects
             s)

# --- Main ---

if __name__ == "__main__":
  funcs = [diapers, brooks, bugs, debt, sir, 
           rework, learn, brooksq, defmap]
  for f in funcs:
    print(f"\n{f.__name__}:")
    show(f())
