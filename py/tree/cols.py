#!/usr/bin/env python3
# vim : nospell filetype=py ts=2 sw=2 sts=2  et  :

from lib import *
from thing import Num,Sym

class Cols(Pretty):
  def __init__(i,inits=[]):
    i.all   = []
    i.nums  = []
    i.syms  = []
    i.names = inits
    i.indep = []
    i.klass = None
    i.xnums = []
    i.xsyms = []
    [i.add(pos,txt) for pos,txt in enumerate(inits)]

  def klassp(i,x):
    return THE.char.klass in x

  def nump(i,x):
    for y in [THE.char.less, THE.char.more, THE.char.num]:    
      if y in x: return True
    return False

  def dep(i,x):
    for y in [THE.char.less, THE.char.more, THE.char.klass]: 
      if y in x: return True
    return False

  def weight(i,x):
    return  -1 if THE.char.less in x else 1

  def add(i,pos,txt):
    klass  = Num if i.nump(txt) else Sym
    tmp    = klass(txt=txt, pos=pos, w=i.weight(txt))
    i.all += [tmp]
    if i.klassp(txt): i.klass=tmp
    what   = i.nums if i.nump(txt) else i.syms
    what  += [tmp]
    if not i.dep(txt):
      i.indep += [tmp]
      what     = i.xnums if i.nump(txt) else i.xsyms
      what    += [tmp]


