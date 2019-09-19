#!/usr/bin/env python3
# vim : nospell filetype=py ts=2 sw=2 sts=2  et  :

from lib import Mine

class o(Mine):
  def __init__(i, **entries): i.__dict__.update(entries)

class THE(Mine):
  sep = ","
  num = "$"
  less = "<"
  more = ">"
  skip = "?"
  klass= "!"
  doomed = r'([\n\t\r ]|#.*)',
  div = o(trivial=1.01, cohen=0.3, min =0.5)


