#!/usr/bin/env python3
# vim : nospell filetype=py ts=2 sw=2 sts=2  et  :

from lib import *

class Row(Pretty):
  def __init__(i, cells=[], cooked=[], dom=0):
    i.cells = cells
    i.cooked = cooked
    i.dom = dom


