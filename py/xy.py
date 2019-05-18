# vim : ts=2 sw=2 sts=2 expandtab:cindent:formatoptions+=cro

def xy(file, ignore= "?", klass = "<>!", 
             sep   = ",", doomed= r'([\n\t\r ]|#.*)'):
  want = lambda z: s[0] != ignore
  goal = lambda z: z[0] in klass
  # --------------------------------------------------------
  # Add all the goal cells to y, the rest to y.
  def xy(src):
    xs, ys = [], []
    for lst in src:
      if not xs:
        for n,s in enumerate(lst):
          what  = ys if goal(s) else xs
          what += [n]
      yield [lst[n] for n in xs], lst[n] for n in ys]
  # --------------------------------------------------------
  # What can compile this string, without crashing?
  def ako(x):
    try: return int(x) and int
    except:
      try: return float(x) and float
      except ValueError: return str
  # --------------------------------------------------------
  # For the first usable thing, work out how to prep it.
  def cells(src):
    fs = None
    def prep(n,x):
      f = fs[n] = fs[n] or ako(x)
      return f(x)
    for lst in src:
      if not fs:
        fs = fs or [None for _ in lst]
        yield lst
      else:
        yield [prep(n,x) for n,x in enumerate(lst)]
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
          lst = txt.split(",") 
          if lst:
            txt = ""
            use = use or [n for n,s in 
                          enumerate(lst) if want(s)] 
            yield [lst[n] for n in use]
  # --------------------------------------------------------
  # main
  for x,y in xy( cells( rows(file))): 
    yield x,y

if __name__ == "__main__":
  for x,y in xy("../data/weather.csv"):
    print(x,y)
