#!/usr/bin/env python3
# vim : nospell filetype=py ts=2 sw=2 sts=2  et  :

class Pretty:
  def __repr__(i):
    pairs = sorted([(k, v) for k, v in i.__dict__.items()
                    if k[0] != "_"])
    pre = i.__class__.__name__ + '{'
    def q(z):
      if isinstance(z,str): return "'%s'" % z
      if callable(z): return "fun(%s)" % z.__name__
      return str(z)
    return pre + ", ".join([('%s=%s' % (k, q(v))) 
                             for k,v in i.__dict__.items()])

class o(Pretty):
  def __init__(i,**d) : i.d().update(**d)
  def d(i)            : return i.__dict__
