# vim : ts=2 sw=2 sts=2 expandtab:cindent:formatoptions+=cro

import re
import sys

def egs(file, no  = "?", klass  = "<>!", 
             sep = ",", doomed = r'([\n\t\r ]|#.*)'):
  want = lambda z: z[0] != no
  goal = lambda z: z[0] in klass

  def xy(src):
    xs, ys = [], []
    for lst in src:
      if not xs:
        for n,s in enumerate(lst):
          what  = ys if goal(s) else xs
          what += [n]
      yield [lst[n] for n in xs], [lst[n] for n in ys]

  def ako(x):
    try: return int(x) and int
    except:
      try: return float(x) and float
      except ValueError: return str

  def cells(src):
    fs = None
    def prep(n,x):
      if x is no: return x
      f = fs[n] = fs[n] or ako(x)
      try: return f(x)
      except ValueError:
        err("wanted %s in col %s, got [%s]",
            ( f.__name__,n,x))
    for lst in src:
      if not fs:
        fs = fs or [None for _ in lst]
        yield lst
      else:
        yield  [prep(n,x) for n,x in enumerate(lst)]
 
  def err(a,b):
    sys.stderr.write(("#E> "+a+"\n")%b)
    sys.exit()
  # --------------------------------------------------------
  # Read rows from file, remove white space and comments,
  # concat togehter lines ending in comma, then return that
  # line, split on 'sep'.
  def rows(file):
    use, txt = [], ""
    with open(file) as fs:
      for line in fs:
        txt += re.sub(doomed, '', line)
        if txt and txt[-1] != sep:
          lst = txt.split(sep) 
          if lst:
            txt = ""
            use = use or [n for n,s in 
                          enumerate(lst) if want(s)] 
            if len(lst) != len(use):
              err("wanted %s cells, got %s in %s", 
                  (len(use), len(lst),lst))
            yield [lst[n] for n in use]
  # --------------------------------------------------------
  for x in xy( cells( rows(file))): 
    yield x

if __name__ == "__main__":
  for x in egs("../data/weather.csv"):
    print(x)
