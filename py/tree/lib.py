#!/usr/bin/env python3
# vim : nospell filetype=py ts=2 sw=2 sts=2  et  :

class Mine:
  oid = 0

  def identify(i):
    Mine.oid += 1
    i.oid = Mine.oid
    return i.oid

  def __repr__(i):
    pairs = sorted([(k, v) for k, v in i.__dict__.items()
                    if k[0] != "_"])
    pre = i.__class__.__name__ + '{'
    q = lambda z: "'%s'" % z if isinstance(z, str) else str(z)
    return pre + ", ".join(['%s=%s' % (k, q(v))
                            for k, v in pairs]) + '}'


