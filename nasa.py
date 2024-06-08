#!/usr/bin/env python3 -B
# <!-- vim: set ts=2 sw=2 sts=2 et: -->
from xomo import of,ints,reals,fill,cprint,settings,cocomo2000
import random

def ground():
  "JPL ground systems"
  return dict(
    newKsloc = reals(11,392),
    adaptedKsloc=0,
    Pmat = of([2,3]),        acap = of([3,4,5]),
    aexp = of([2,3,4,5]),    cplx = of([1,2,3,4]),
    data = of([2,3]),        rely = of([1,2,3,4]),
    ltex = of([1,2,3,4]),    pcap = of([3,4,5]),
    pexp = of([1,2,3,4]),    time = of([3,4]),
    stor = of([3,4]),
    tool = of([2]),
    sced = of([3]))

def flight():
  "JPL flight systems"
  return dict(
    newKsloc = reals(4,418),
    adaptedKsloc=0,
    revl=0,
    Pmat = of([2,3]),        acap = of([3,4,5]),
    apex = of([2,3,4,5]),    cplx = of([3,4,5,6]),
    data = of([2,3]),        ltex = of([1,2,3,4]),  
    pcap = of([3,4,5]),      plex = of([1,2,3,4]),
    rely = of([3,4,5]),      sced = of([3]),
    stor = of([3,4]),        time = of([3,4]),
    tool = of([2]))
    
def osp():
  "Orbital space plane. Flight guidance system."
  return dict(
    newKsloc = reals(75,125),
    adaptedKsloc=0,
    Flex = of([2,3,4,5]),    Pmat = of([1,2,3,4]),
    Prec = of([1,2]),        Resl = of([1,2,3]),
    Team = of([2,3]),        acap = of([2,3]),
    aexp = of([2,3]),        cplx = of([5,6]),
    docu = of([2,3,4]),      ltex = of([2,3,4]),
    pcon = of([2,3]),        tool = of([2,3]),
    ruse = of([2,3,4]),      sced = of([1,2, 3]),
    stor = of([3,4,5]),      data = of([3]),
    pcap = of([3]),          pexp = of([3]),
    pvol = of([2]),          rely = of([5]),
    site = of([3]))

def osp2():
  """Osp, version 2. Note there are more restrictions
  here than in osp version1 (since as a project
  develops, more things are set in stone)."""
  return dict(
    newKsloc = reals(75,125),
    adaptedKsloc=0,
    docu = of([3,4]),         ltex = of([2,5]),
    sced = of([2,3,4]),       Pmat = of([4,5]),
    Prec = of([3,4, 5]),
    Resl = of([4]),           Team = of([3]),
    acap = of([4]),           aexp = of([4]),
    cplx = of([4]),           data = of([4]),
    Flex = of([3]),           pcap = of([3]),
    pcon = of([3]),           pexp = of([4]),
    pvol = of([3]),           rely = of([5]),
    ruse = of([4]),           site = of([6]),
    stor = of([3]),           time = of([3]),
    tool = of([5]))

def doNothing(): return {}

def improvePersonnel(): return dict(
  acap=5,pcap=5,pcon=5, aexp=5, pexp=5, ltex=5)

def improveToolsTechniquesPlatform(): return dict(
  time=3,stor=3,pvol=2,tool=5, site=6)

def improvePrecendentnessDevelopmentFlexibility(): return dict(
  Prec=5,Flex=5)

def increaseArchitecturalAnalysisRiskResolution(): return dict(
  Resl=5)

def relaxSchedule(): return dict(
  sced = 5)

def improveProcessMaturity(): return dict(
  Pmat = 5)

def reduceFunctionality(): return dict(
  data = 2, reduceLoc=0.5) # nloc is a special symbol. Used to change kloc.

def improveTeam(): return dict(
  Team = 5)

def reduceQuality():  return dict(
  rely = 1, docu=1, time = 3, cplx = 1)

rx = dict( doNothing=doNothing, improvePersonel=improvePersonnel, 
       improveTools=improveToolsTechniquesPlatform,
       improvePrec=improvePrecendentnessDevelopmentFlexibility,
       moreArch=increaseArchitecturalAnalysisRiskResolution, 
       releaxSchedule=relaxSchedule,
       improveProcess=improveProcessMaturity, 
       reduceFunctioanlity=reduceFunctionality, 
       improveTeam=improveTeam,
       reduceQuality=reduceQuality)

def mid(lst): n=len(lst); lst.sort();return lst[n//2]
def div(lst): n=len(lst)//10; lst.sort(); return (lst[9*n] - lst[n])/2.58

if __name__ == "__main__":
  the = settings()
  random.seed(the.seed)
  print(the.seed, random.random())
  for k,rx in rx.items():
    m,r=[],[]
    for _ in range(the.repeats):
      months,timE,staff,risks = cocomo2000(fill(flight,rx))
      m += [months]
      r += [risks]
    print(f"{k:20} {mid(m):10.3f} {mid(r):10.0f} {div(m):10.3f} {div(r):10.3f}")
