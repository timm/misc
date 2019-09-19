#!/usr/bin/env python3
# vim : nospell filetype=py ts=2 sw=2 sts=2  et  :

import re,sys
from lib import Mine
from the import THE
from thing import Num,Sym

class Meta(Mine):
  def __init__(i):
    i.klass=None
    i.xnums=[]
    i.xsyms=[]
    i.nums=[]
    i.syms=[]

class Tbl(Mine):
  def __init__(i):
    i.rows=[]
    i.cols=[]
    i.my  = Meta()
    i.identify()  

  def what(i,pos,txt):
    def nump():
      for y in [THE.less, THE.more, THE.num]:    
        if y in txt: return True
      return False
    def indep():
      for y in [THE.less, THE.more, THE.klass]: 
        if y in txt: return False
      return True
    w       = -1 if THE.less in txt else 1
    klass   = Num if nump() else Sym
    new     = klass(txt=txt,pos=pos,w=w)
    i.cols += [new]
    what    = i.my.nums if nump() else i.my.syms
    what   += [new]
    if indep():
       what  = i.my.xnums if nump() else i.my.xsyms
       what += [new]
    if THE.klass in txt: i.my.klass = new
    return new 

  def read(i, src):
    for n, lst in enumerate(cells(cols(rows(src)))):
      if n == 0:  # check init title list is empty
        i.cols = [i.what(pos,txt) for pos,txt in enumerate(lst)]
      else:
        [col + x for col,x in zip(i.cols,lst)]
        i.rows += [Row(lst)]

class Col(Mine):
  def __init__(i, pos=0, txt=None):
    i.identify()
    i.pos = pos
    i.txt = txt

class Row(Mine):
  def __init__(i, cells=[], cooked=[], dom=0):
    i.identify()
    i.cells = cells
    i.cooked = cooked
    i.dom = dom


def string(s):
  """read lines from a string"""
  for line in s.splitlines():
    yield line


def rows(src):
  """convert lines into lists, killing whitespace
  and comments. skip over lines of the wrong size"""
  linesize = None
  for n, line in enumerate(src):
    line = re.sub(THE.doomed, '', line.strip())
    if line:
      line = line.split(THE.sep)  # breakup a string and add the data to a string array
      if linesize is None:
        linesize = len(line)
      if len(line) == linesize:
        yield line
      else:
        print("E> skipping line %s" % n, file = sys.stderr)  # To print to STDERR

def cols(src):
  """skip columns whose name contains '?'"""
  usedCol = None
  for cells in src:
    # usedCol = usedCol or [n for n, cell in enumerate(cells) if not THE.skip in cell]
    if usedCol is None:
      usedCol = [n for n, cell in enumerate(cells) if not THE.skip in cell]
    yield [cells[n] for n in usedCol]

def cells(src):
  """convert strings into their right types"""
  one = next(src)
  fs = [None] * len(one)  # [None, None, None, None]
  yield one  # the first line
  def ready(n, cell):
    if cell == THE.skip:
      return cell  # skip over '?'
    fs[n] = fs[n] or prep(one[n])  # ensure column 'n' compiles
    return fs[n](cell)  # compile column 'n'
  for _, cells in enumerate(src):
    yield [ready(n, cell) for n, cell in enumerate(cells)]


def prep(x):
  def num(z):
    f = float(z)
    i = int(f)
    return i if i == f else f
  for c in [THE.num, THE.less, THE.more]:
    if c in x:
      return num
  return str


