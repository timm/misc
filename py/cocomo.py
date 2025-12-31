#!/usr/bin/env python3 -B
# cocoon.py — minimal, readable, 80-col, risk tables on top
from random import uniform,seed

def has(ako, lo=1, hi=5): return o(ako=ako, lo=lo, hi=hi)

def coc2():
  p, n, s = "+", "-", "*"
  return o(
    # Effort Multipliers (1..5 or 1..6)
    Acap=has(n),       Aexp=has(n),       Pcap=has(n),
    Pcon=has(n),       Plex=has(n),       Ltex=has(n),
    Tool=has(n),       Sced=has(n),       Rely=has(p),
    Docu=has(p),       Cplx=has(p, 1, 6),

    # Scale Factors (1..6)
    Prec=has(s, 1, 6), Flex=has(s, 1, 6), Arch=has(s, 1, 6),
    Team=has(s, 1, 6), Pmat=has(s, 1, 6),

    # Special Cases / Drivers
    Site=has(n, 1, 6), Data=has(p, 2, 5), Pvol=has(p, 2, 5),
    Ruse=has(p, 2, 6), Time=has(p, 3, 6), Stor=has(p, 3, 6),

    # Size and Modifiers 
    KLOC       = has("=", 2, 2000), # KSLOC Range
    KLOC_times = has("=", 1, 100)   # Modifier
  )

def policies(): return o(
  Noop     = proj(coc2(), o()),
  Arch     = proj(coc2(), o(Arch=5)),
  MoreTime = proj(coc2(), o(Sced=5)),
  MoreProc = proj(coc2(), o(Pmat=5)),
  Team     = proj(coc2(), o(Team=5)),
  Prec     = proj(coc2(), o(Prec=5, Flex=5)),
  DoLess   = proj(coc2(), o(Data=2, KLOC_times=50)),
  LoQual   = proj(coc2(), o(Rely=1, Docu=1, Time=3, Cplx=1)),
  Tools    = proj(coc2(), o(Time=3, Stor=3, Pvol=2, Tool=5, Site=6)),
  Pers     = proj(coc2(), o(Acap=5, Pcap=5, Pcon=5, Aexp=5, Plex=5, 
                            Ltex=5)),
)

def projects(): return o(
  OSP = proj(coc2(), o(
    KLOC=(75, 125),   KLOC_times=100,
    Prec=(1, 2),      Flex=(2, 5),    Arch=(1, 3),    Team=(2, 3), 
    Pmat=(1, 4),      Stor=(3, 5),    Ruse=(2, 4),    Docu=(2, 4),
    Cplx=(5, 6),      Data=3,         Pvol=2,         Rely=5,      
    Acap=(2, 3),      Pcon=(2, 3),    Aexp=(2, 3),    Ltex=(2, 4), 
    Tool=(2, 3),      Sced=(1, 3),    Pcap=3,         Plex=3, 
    Site=3
  )),
  OSP2 = proj(coc2(), o(
    KLOC=(75, 125),   KLOC_times=100,
    Prec=(3, 5),      Pmat=(4, 5),    Flex=3,         Arch=4,      
    Team=3,           Docu=(3, 4),    Ltex=(2, 5),    Sced=(2, 4),
    Time=3,           Stor=3,         Data=4,         Pvol=3,      
    Ruse=4,           Rely=5,         Cplx=4,         Acap=4,      
    Pcap=3,           Pcon=3,         Aexp=4,         Plex=4,      
    Tool=5,           Site=6
  )),
  JPL_Flight = proj(coc2(), o(
    KLOC=(7, 418),    KLOC_times=100,
    Pmat=(2, 3),      Rely=(3, 5),    Data=(2, 3),    Cplx=(3, 6),
    Time=(3, 4),      Stor=(3, 4),    Acap=(3, 5),    Aexp=(2, 5), 
    Pcap=(3, 5),      Plex=(1, 4),    Ltex=(1, 4),    Tool=2,      
    Sced=3
  )),
  JPL_Ground = proj(coc2(), o(
    KLOC=(11, 392),   KLOC_times=100,
    Pmat=(2, 3),      Rely=(1, 4),    Data=(2, 3),    Cplx=(1, 4),
    Time=(3, 4),      Stor=(3, 4),    Acap=(3, 5),    Aexp=(2, 5), 
    Pcap=(3, 5),      Plex=(1, 4),    Ltex=(1, 4),    Tool=2,      
    Sced=3
  ))
)


_ = 0

ne = [
  [_, _, _, 1, 2, 4],
  [_, _, _, _, 1, 2],
  [_, _, _, _, _, 1],
  [_, _, _, _, _, _],
  [_, _, _, _, _, _],
  [_, _, _, _, _, _]]

ne86 = [
  [_, _, 1, 2, 4, 8],
  [_, _, _, 1, 2, 4],
  [_, _, _, _, 1, 2],
  [_, _, _, _, _, 1],
  [_, _, _, _, _, _],
  [_, _, _, _, _, _]]

nw = [
  [4, 2, 1, _, _, _],
  [2, 1, _, _, _, _],
  [1, _, _, _, _, _],
  [_, _, _, _, _, _],
  [_, _, _, _, _, _],
  [_, _, _, _, _, _]]

nw8 = [
  [8, 4, 2, 1, _, _],
  [4, 2, 1, _, _, _],
  [2, 1, _, _, _, _],
  [1, _, _, _, _, _],
  [_, _, _, _, _, _],
  [_, _, _, _, _, _]]

sw = [
  [_, _, _, _, _, _],
  [1, _, _, _, _, _],
  [2, 1, _, _, _, _],
  [4, 2, 1, _, _, _],
  [_, _, _, _, _, _],
  [_, _, _, _, _, _]]

sw8 = [
  [_, _, _, _, _, _],
  [1, _, _, _, _, _],
  [2, 1, _, _, _, _],
  [4, 2, 1, _, _, _],
  [8, 4, 2, 1, _, _],
  [_, _, _, _, _, _]]

sw26 = [
  [_, _, _, _, _, _],
  [_, _, _, _, _, _],
  [1, _, _, _, _, _],
  [2, 1, _, _, _, _],
  [4, 2, 1, _, _, _],
  [_, _, _, _, _, _]]

sw86 = [
  [_, _, _, _, _, _],
  [1, _, _, _, _, _],
  [2, 1, _, _, _, _],
  [4, 2, 1, _, _, _],
  [8, 4, 2, 1, _, _],
  [_, _, _, _, _, _]]

def risks(): return o(
  Ltex=o(Pcap=nw8),
  Pvol=o(Plex=sw),

  Pmat=o(Acap=nw, Pcap=sw86),
  Ruse=o(Aexp=sw86, Ltex=sw86),
  Stor=o(Acap=sw86, Pcap=sw86),

  Cplx=o(Acap=sw86, Pcap=sw86, Tool=sw86),
  Rely=o(Acap=sw8, Pcap=sw8, Pmat=sw8),
  Team=o(Aexp=nw, Sced=nw, Site=nw),
  Time=o(Acap=sw86, Pcap=sw86, Tool=sw26),
  Tool=o(Acap=nw, Pcap=nw, Pmat=nw),

  Sced=o(
    Cplx=ne86, Time=ne86, Pcap=nw8, Aexp=nw8, Acap=nw8,
    Plex=nw8, Ltex=nw, Pmat=nw, Rely=ne, Pvol=ne, Tool=nw
  )
)

#--- dsl code
class o(dict):
  __getattr__ = dict.get
  __setattr__ = dict.__setitem__
  def __repr__(self):
    isa = isinstance
    def say(v):
      if isa(v, float): return f"{v:,.2f}".rstrip('0').rstrip('.')
      if isa(v, dict):  return o(v)
      if isa(v, (list, tuple)): return ', '.join(say(x) for x in v)
      return str(v)
    return "{" + " ".join(f":{k} {say(v)}" 
                          for k, v in sorted(self.items())) + "}"

def from_(lo, hi): return uniform(lo, hi)
def within(x, lo, hi): return max(lo, min(hi, x))
def int_(x): return int(x+0.5)

def proj(b4, also):
  def check(k, v):
    if isinstance(v, o): lo, hi = v.lo, v.hi
    elif isinstance(v, (list, tuple)): lo, hi = v[0], v[1]
    else: lo, hi = v, v
    if k not in b4:
      if k in ["KLOC", "KLOC_times"]: return has("=", lo, hi)
      raise ValueError(f"Unknown key: {k}")
    # Policy (b4) is ground truth. Clamp Project (also) into it.
    p = b4[k]
    return has(p.ako, within(lo, p.lo, p.hi), within(hi, p.lo, p.hi))
  return o(b4 | {k: check(k, v) for k, v in also.items()})

#-- coocmo logic

def emsf(sign, z):
  # Effort Multipliers (EM) center around 1.0 at z=3
  if sign == "+": return 1.0 + (z-3)*from_(0.073, 0.21)
  if sign == "-": return 1.0 + (z-3)*from_(-0.187, -0.078)
  # Scale Factors (SF) should be small positive increments
  # Usually z=6 is 'Extra High' (0.0) and z=1 is 'Very Low'
  return (6-z) * from_(1.0, 1.5) 

def effort(coc, y):
  em, sf = 1.0, 0.0
  for k, t in coc.items():
    if k in ["KLOC", "KLOC_times"]: continue
    v = y[k]
    if t.ako in ("+", "-"): em *= v
    elif t.ako == "*": sf += v
  # Exponent calculation: b + 0.01 * sum(SF)
  return y.a * (y.kloc ** (y.b + 0.01*sf)) * em

def cocoon(proj, risk):
  x, y = o(), o()
  for k, t in proj.items():
    x[k] = int_(from_(t.lo, t.hi))
    y[k] = emsf(t.ako, x[k])
  y.a = from_(2.3, 9.18)
  y.b = ((0.85 - 1.1) / (9.18 - 2.3)) * (y.a - 2.3) + 1.1
  y.kloc = x.KLOC * x.get("KLOC_times", 100)/100
  y.effort = effort(proj, y)
  y.risk = 100*sum(risk[a][b][x[a]-1][x[b]-1] 
                   for a in risk for b in risk[a]
                   if risk[a][b][x[a]-1][x[b]-1]) / 216
  return o(x=x, y=y)

def checks():
  import statistics
  def median(lst): return statistics.median(lst)
  seed(1)
  # Setup baseline
  no_policy = policies().Noop
  jpl = projects().JPL_Ground
  
  def get_median_effort(pol, pro):
    return median([cocoon(proj(pol, pro), risks()).y.effort 
                   for _ in range(50)])

  print("--- RUNNING CHECKS ---")
  
  # 1. DoLess vs Noop
  do_less = get_median_effort(policies().DoLess, jpl)
  noop = get_median_effort(no_policy, jpl)
  print(f"01. DoLess logic: {do_less < noop} (DoLess: {do_less:,.0f} vs Noop: {noop:,.0f})")

  # 2. Pers (High Cap) vs Noop
  pers = get_median_effort(policies().Pers, jpl)
  print(f"02. Pers logic:   {pers < noop} (Pers: {pers:,.0f} vs Noop: {noop:,.0f})")

  # 3. Complex Penalty
  lo_qual = get_median_effort(policies().LoQual, jpl)
  print(f"03. Cplx penalty: {lo_qual < noop} (LoQual: {lo_qual:,.0f} vs Noop: {noop:,.0f})")

  # 4. JPL Scale Check (Flight vs Ground)
  flight = get_median_effort(no_policy, projects().JPL_Flight)
  print(f"04. Scale check:  {flight > noop} (Flight: {flight:,.0f} vs Ground: {noop:,.0f})")

  # 5. Type Check (No Complex Numbers)
  xy = cocoon(proj(no_policy, jpl), risks())
  print(f"05. Real numbers: {not isinstance(xy.y.effort, complex)}")

  # 6. Risk Range Check
  risky = [cocoon(proj(no_policy, jpl), risks()).y.risk 
           for _ in range(100)]
  print(f"06. Risk bounds:  {min(risky) >= 0 and max(risky) <= 1}")

  # 7. Clamping Check
  clamped = proj(no_policy, {"Tool": 10}) # 10 is out of bounds
  print(f"07. Clamp check:  {clamped.Tool.hi <= 5} (Target 10 -> Result {clamped.Tool.hi})")

  # 8. KLOC Pass-through
  print(f"08. KLOC check:   {'KLOC' in proj(no_policy, jpl)}")

  # 9. Noop Variance (Should be larger than specific policy variance)
  def variance(pol, pro):
    data = [cocoon(proj(pol, pro), risks()).y.effort 
            for _ in range(50)]
    return max(data) - min(data)
  print(f"09. Var check:    {variance(no_policy, jpl) > variance(policies().Pers, jpl)}")

  # 10. Seed Reproducibility
  seed(42)
  val1 = cocoon(proj(no_policy, jpl), risks()).y.effort
  seed(42)
  val2 = cocoon(proj(no_policy, jpl), risks()).y.effort
  print(f"10. Seed check:   {val1 == val2}")

def sample():
  """
  In Noop, large differences in effort ;e e g osp Nop 2150 to 5243.
  As  project marture, ceratain risk factors are reduced; e.g. osp2 
     has much mower risj tthan osp
  Potential for optiization very arge (eg. osp2, noop to pers max 
     effor 1125 ==> 176. this 'odereds
     of magnitice' often commente in the ltiertature; ere iwe see it clearly 
     demonstrated in a repdocaible way.
  do general patterns always ho=old i specific projects:: need to
     check with rule elarner
  """
  seed(1)
  for k1,pro in projects().items():
    print("\n"+k1)
    for k2,pol in policies().items():
      print("  "+k2)
      tmp=[cocoon(proj(pol,pro),risks()).y for _ in range(10)] 
      print("    ",o(effort=sorted(map(int,(z.effort for z in tmp)))))
      print("    ",o(risk  =sorted(map(int,(z.risk for z in tmp)))))

if __name__ == "__main__":
  checks(); sample()
  
