import re

# init a thing
# decide type in things
# ocer ttype in stings
# return a row with ids and x and y
# i need x,y

def atom(x)  :
  try: return int(x), int
  except:
    try: return float(x), float
    except ValueError: return x, str

class egs(object):
  def __init__(i):
    i.klass  = None
    i.x, i.y, i.less, i.more, i.all = [],[],[],[],[]
  def rows(file):
    """kill whitespace, comments, skip blanklines, join lines
       ending with ',', return list of cells, one per line,
       skips columns marked '?' in column one.i
       convert those string to nums as needed"""
    usep   = lambda z: z[0] != '?'
    same   = lambda z: z
    pre    = lambda z: float if z[0] in '<>$' else same 
    txt,fs = "", None
    with open(file) as fs:
      for line in fs:
        txt += re.sub(r'([\n\t\r ]|#.*)', '', line)
        if txt and txt[-1] != ",":
          lst = txt.split(",") 
          if lst:
            txt = ""
            fs  = fs or [ (n,pre(s)) for n,s in 
                          enumerate(lst) if usep(s) ]
            yield [ f(lst[n]) for n,f in fs]

class thing(object):
   """The first thing seen is the name.
      The other things are either things to ignore 
      or things we should use in training"""
   less   = "<"
   more   = ">"
   klass  = "!"
   ignore = "?"
   def __init__(i,name="", inside=egs()): 
     i.egs, i.name, i.it = inside, name, None
     what = i.egs.y
     if   thing.less  in name : i.egs.less += [self]
     elif thing.more  in name : i.egs.more += [self]
     elif thing.klass in name : i.egs.klass = self
     else                     : what = i.egs.x
     what  += [self]
     i.all += [self]
   def train(i, z, n=1):
     if z is thing.ignore: return z
     if not i.it:
        z, i.what = atom(z)
        i.it      = bins() if i.what==str else around()
     i.it.train(z, n)

# need a bins for ints


