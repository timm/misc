#!/usr/bin/env python3
# vim : nospell filetype=py ts=2 sw=2 sts=2  et  :

import sys
om lib import *

def rows(src):
  """convert lines into lists, killing whitespace
  and comments. skip over lines of the wrong size"""
  linesize = None
  for n, line in enumerate(src):
    line = re.sub(THE.char.doomed, '', line.strip())
    if line:
      # breakup a string and add the data to a string array
      line = line.split(THE.char.sep)  
      if linesize is None:
        linesize = len(line)
      if len(line) == linesize:
        yield line
      else:
        print("E> skipping line %s" % n, file = sys.stderr)  

def cols(src):
  "skip columns whose name contains '?'"
  usedCol = None
  for cells in src:
    if usedCol is None:
      usedCol = [n for n, cell in enumerate(cells) 
                if not THE.char.skip in cell]
    yield [cells[n] for n in usedCol]

def cells(src):
  "convert strings into their right types"
  one = next(src)
  fs = [None] * len(one)           # [None, None, None, None]
  yield one                        # the first line
  def ready(n, cell):
    if cell == THE.char.skip:
      return cell                  # skip over '?'
    fs[n] = fs[n] or prep(one[n])  # ensure column 'n' compiles
    return fs[n](cell)             # compile column 'n'
  for _, cells in enumerate(src):
    yield [ready(n, cell) for n, cell in enumerate(cells)]

def prep(x):
  "return a function that can compile things of type 'x'"
  def num(z):
    f = float(z)
    i = int(f)
    return i if i == f else f
  for c in [THE.char.num, THE.char.less, THE.char.more]:
    if c in x:
      return num
  return str
