# vim: ts=2 sw=2 sts=2 expandtab:cindent:formatoptions+=cro
def worthy():
  r    = random.random
  any  = random.choice
  more =  1
  less = -1

  class o:
    def __init__(i, **d): i.__dict__.update(d)
  class have(o): 
    def __call__(i): return [i.eg() for _ in range(i.n)]

  def weight()             : return any([more,less])
  def num(lo=0, hi=1)      : return lo+r()*(hi - lo)
  def sym(lst=[True,False]): return any(lst)

  def thing():
   return o(x= o( nums= have(n=5,eg=num),
                  syms= have(n=2,eg=sym)),
            y=  o( nums= have(n=3,eg=num),
                   ws  = have(n=2,eg=weight)))

  def eg(d) : 
    if type(d) == "have": return d()
    if type(d) != "o"  : return d
    return {(k:example(v) for k,v in d.items())}

  def egs(n,about): 
    return have(n=n,eg= lambda: eg(about))()

  about = thing()
  p=0
  pop=[eg(n,about)]
