import re

# init a thing
# decide type in things
# ocer ttype in stings
# return a row with ids and x and y
# i need x,y

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
