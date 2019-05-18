# vim: ts=2 sw=2 sts=2 expandtab:cindent:formatoptions+=cro
#---------1---------2---------3---------4---------5---------
def xy(file, ignore="?", klass="<>!"):
  want = lambda z: s[0] != ignore
  goal = lambda z: z[0] in klass
  #---------------------------------------------------------
  def xy(src):
    xs, ys = [], []
    for lst in src:
      if not xs:
        for n,s in enumerate(lst):
          what  = ys if goal(s) else xs
          what += [n]
      yield [lst[n] for n in xs], lst[n] for n in ys]
  #---------------------------------------------------------
  def ako(x):
    try: return int(x) and int
    except:
      try: return float(x) and float
      except ValueError: return str
  #---------------------------------------------------------
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
  #---------------------------------------------------------
  def rows(file):
    use, txt = [], ""
    with open(file) as fs:
      for line in fs:
        txt += re.sub(r'([\n\t\r ]|#.*)', '', line)
        if txt and txt[-1] != ",":
          lst = txt.split(",") 
          if lst:
            txt = ""
            use = use or [n for n,s in 
                          enumerate(lst) if want(s)] 
            yield [lst[n] for n in use]
  #---------------------------------------------------------
  for x,y in xy( cells( rows(file))): 
    yield x,y

if __name__ == "__main__":
  for x,y in xy("../data/weather.csv"):
    print(x,y)
