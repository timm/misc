#!/usr/bin/env python3 -B
# vim : nospell filetype=py ts=2 sw=2 sts=2  et  :

from tbl import *

t=Tbl()
t.read('weathernom.csv')
for x in t.cols.all:
  print(x)
