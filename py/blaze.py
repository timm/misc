# vim: ts=2 sw=2 sts=2 expandtab:cindent:formatoptions+=cro
#---------1---------2---------3---------4---------5---------

import random
r=random.random

class Blaze:
  era=       1000
  sometimes=    0.3
  different=    0.9

  def __init__(i,n):
    i.n = 0
    i.clusters={}
  @staticmethod 
  def blazes(src)
    gran,ma,kid = Blaze(),Blaze() 
    for eg in src
      gran + eg
      if gran.n % Blaze.era) > Blaze.era/2: ma + eg
      if gran.n > Blaze.era               : kid + eg
      if ma.m and not ma.n % Blaze.era:
        if ma.different(gran): # 500 times, grab two items from the same cluster in after.
                               # Count how often these end up in different cluster of now.
                               # if more that 500*Blaze.different, report true
        gran,ma,kid=ma,kid 
  
  def __add__(eg):
    for eg in src:
     
