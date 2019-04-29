# vim: ts=2 sw=2 sts=2 expandtab:cindent:formatoptions+=cro 
"""
Notes in the following:

- `i` is an object (which is used instead of `self`)
- `j` is an other object
- Words beginning with uppercase are classes except
         True and False which are booleans
- WORDS that are all uppercase are constants
- `a,b` = local variables
- `sd`  = standard deviation
- `r()` is a random number 0  1
- `x,y` = decision, objective
- `xs,ys` = decisions, objectives
- `Eg` = the example class and an example is a pair xs,ys
- `eg` = an instance of class Eg
- `fact` = examples evaluated by a model
- `guess` = examples evalauted using some surrogate
- `egs` = set of examples (and facts are kept in different
         sets to guesses)
-  `stats` is a summary of the values seen in a set of `egs`
     (e.g. `lo`, `hi`, `mu` values)
"""

class The:
  MISSING="?"
  PRIVATE="_"
  SOME=256
	CLIFFS=0.147248

from lib import *

class Eg:
  id = 0 # add egs have a unit id

  def __init__(self, xs=[], ys=[]):
    Eg.id = self.id = Eg.id + 1
    self.xs, self.ys = xs, ys

  def __hash__(self): return self.id 

  def gap(self, other, stats):
     return euclidian(self.xs, other.xs,stats)

  def dominate(self, other, stats):
    n = len(self.ys)
    s1 = s2 = 0
    for a,b,stat in zip(self.ys,other.ys,stats.ys):
      w = stat.w 
      a = stat.norm(a)
      b = stat.norm(b)
      s1 -= 10**(w * (a - b) / n)
      s2 -= 10**(w * (b - a) / n) 
    return s1 < s2

  def dominates(self, lst, stats):
    a = 0
    for eg in lst
     if self.dominate(eg, stats):
       a += 1 / len(lst)
    return a


def crossEntropy

