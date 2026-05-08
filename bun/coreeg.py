#!/usr/bin/env python3 -B
"""
coreeg.py: core library demos / smoke tests.
"""
from pathlib import Path
from core import *

egopt = str(Path.home() / "gits/moot/optimize/misc/auto93.csv")

def test_h():
  """Print help."""
  import core; print(__doc__); print(core.__doc__)

def test_the():
  """Show parsed config."""
  print(o(the))

def test_list():
  """List demos."""
  for k, fn in sorted(globals().items()):
    if k.startswith("test_"):
      print(f"  --{k[5:]:12} {fn.__doc__ or ''}")

def test_o():
  """Format scalars/dicts/objects."""
  print(o(3.14159))
  print(o([1, {"a": 2}, S(x=1, y=2)]))

def test_table():
  """Aligned table."""
  table([{"name": "tim", "age": 21, "shoe": 10},
         {"name": "tom", "age": 22, "shoe": 9.5}], w=8)

def test_thing():
  """Coerce strings."""
  assert thing("3.14") == 3.14
  assert thing(" true ") in (True, 1)
  assert thing(" false ") in (False, 0)
  assert thing("hello") == "hello"
  print("ok")

def test_nest():
  """Nested namespace."""
  t = S(); nest(t, "a.b.c", 42)
  assert t.a.b.c == 42; print(o(t))

def test_csv(file=egopt):
  """CSV reader."""
  assert len(list(csv(file))) > 10

def test_num():
  """Num running stats."""
  c = adds([10, 20, 30, 40, 50], Num())
  assert c.mu == 30 and 15.8 < spread(c) < 15.9

def test_sym():
  """Sym entropy."""
  c = adds("aaabbc", Sym())
  assert mid(c) == "a" and 1.4 < spread(c) < 1.5

def test_pick():
  """Sample from Num/Sym."""
  c1 = adds([10, 20, 30, 40, 50], Num())
  c2 = adds(pick(c1) for _ in range(10000))
  assert abs(mid(c1) - mid(c2)) < 0.25
  c1 = adds("aaabbc", Sym())
  c2 = adds([pick(c1) for _ in range(1000)], Sym())
  assert mid(c1) == mid(c2)

def test_cols():
  """Col role + heaven."""
  cols = Cols(["name", "Age", "Weight-"])
  assert not cols.ys[0].heaven

def test_data(file=egopt):
  """Data from CSV."""
  d = Data(csv(file))
  assert len(d.rows) > 0 and len(d.cols.ys) > 0

def test_addsub(file=egopt):
  """add/sub keep running stats consistent."""
  d, d2 = Data(csv(file)), clone(Data(csv(file)))
  m1 = m2 = None
  for r in d.rows:
    add(d2, r)
    if len(d2.rows) == 50: m1 = mids(d2)
  for r in d.rows[::-1]:
    sub(d2, r)
    if len(d2.rows) == 50: m2 = mids(d2)
  assert all(abs(a - b) < 0.01 for a, b in zip(m1, m2))

def test_distx(file=egopt):
  """X-distance sort."""
  d, r1 = Data(csv(file)), Data(csv(file)).rows[0]
  for r in sorted(d.rows, key=lambda r2: distx(d, r1, r2))[::30]:
    print(*r, sep="\t")

def test_disty(file=egopt):
  """Y-distance sort."""
  d = Data(csv(file))
  for r in sorted(d.rows, key=lambda r: disty(d, r))[::30]:
    print(*r, ":", round(disty(d, r), 2), sep="\t")

if __name__ == "__main__": cli()
