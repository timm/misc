#!/usr/bin/env python3
# vim : nospell filetype=py ts=2 sw=2 sts=2  et  :

from lib   import *
from thing import Num,Sym
from cols  import Cols
from row   import Row
from tbl_iterators import cells,cols,rows

class Tbl(Pretty):
  def __init__(i,cols=None):
    i.rows = []
    i.cols = cols

  def clone(i):
    return   Tbl( Cols(i.cols.names) )

  def read(i, src):
    for lst in cells(cols(rows(src))):
      if i.cols:
        lst = [col + x for col,x in zip(i.cols.all,lst)]
        i.rows += [Row(lst)]
      else:
        i.cols = Cols(lst)
