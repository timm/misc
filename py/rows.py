import re

# init a thing
# decide type in things
# ocer ttype in stings
# return a row with ids and x and y
# i need x,y

class eg(object):
   id=0
   def __init__(i, egs, xs=[], ys=[]): 
     eg.id = i.id = eg.id + 1
     i.xs, i.ys, i.egs = xs, ys, egs

class egs(object):
  ignore = "?"
  less   = "<"
  more   = ">"
  klass  = "!"
  ys     = "<>!"
  def __init__(i):
    i.klass  = None
    i.xs, i.ys, i.less, i.more = [],[],[],[]
  def also(i,c):
     if    egs.less  in c.name : i.less  += [c]
     elif  egs.more  in c,name : i.more  += [c]
     elif  egs.klass in c.name : i.klass  = c
     return c
  def rows(i,file):
    for cnt,(xs,ys) in enumerate(xy(rows(file))):
      if cnt==0:
        i.xs = [       col(x,n)  for n,x in enumerate(xs)]
        i.ys = [i.also(col(y,n)) for n,y in enumerate(ys)]
      else:
        yield eg(i, xs=xs, ys=ys)

class col(object):
  def __init__(i,  name,pos): 
    i.name, i.pos = name, pos
    i.log  = None # place to stats about a number
    i.prep = None # coerce function string to something
  def train(i, z, n=1):
    if z is egs.ignore: return z
    if not i.log:
       z, i.prep = atom(z)
       i.log     = bins() if i.prep == str else around()
    i.log.train( pre(z), n )


