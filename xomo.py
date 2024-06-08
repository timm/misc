#!/usr/bin/env python3 -B
# <!-- vim: set ts=2 sw=2 sts=2 et: -->
import sys,random,argparse

def of(x):     return random.choice(x)
def ints(a,b): return random.randint(a,b)
def reals(a,b): return random.uniform(a,b)

class o:
  def __init__(i,**d): i.__dict__.update(d)
  def __repr__(i): return i.__class__.__name__ + str(i.__dict__)
 
def base():
    return dict(
    # calibration params
    a       = reals(2.25,3.25), # tuning for linear effects
    b       = reals(0.9, 1.1),  # tuning for exponential effects   
    c       = reals(3.0, 3.67), # used in "tdev"
    d       = reals(0.28, 0.33), # used in exponent of "tdev:
    # size params
    newKsloc = ints(2, 10000),# new code, thousands of lines of codes
    adaptedKsloc = ints(2,10000),
    # percentages
    revl    = ints(0,100), # percentage of code discarded
    aa      = ints(0,100), # degree of assessment and assimilation
    su      = ints(0,100), # the software understanding
    unfm    = ints(0,100), # programmers' unfamiliarity with the software
    dm      = ints(0,100), # percent of adjusted softwaer design taht is modified
    cm      = ints(0,100), # percent of code modified
    at      = ints(0,100), # percent of code reengineered via auto translation
    im      = ints(0,100), # percent of effort ncessary to integrate reused softwre
    atKProd = reals(1E-10,100), # percent inside pmAuto
    # Scale factors
    Prec    = ints(1, 5), # Precedentedness
    Flex    = ints(1, 5), # Development Flexibility
    Resl    = ints(1, 5), # Architecture/Risk Resolution
    Team    = ints(1, 5), # Team Cohesion
    Pmat    = ints(1, 5), # Process Maturity
    #--- effort multipliers: linear effect on effort ----
    # Product Factors:
    rely    =	ints(1, 5), # Required Software Reliability 
    data    =	ints(2, 5), # Data Base Size
    cplx    =	ints(1, 6), # Product Complexity
    ruse    =	ints(2, 6), # Required Reusability
    docu    =	ints(1, 5), # Documentation Match to Life-Cycle Needs
    # Platform Factors:
    time    =	ints(3, 6), # Execution Time Constraint
    stor    =	ints(3, 6), # Main Storage Constraint
    pvol    =	ints(2, 5), # Platform Volatility
    # Personnel Factors:
    acap    =	ints(1, 5), # Analyst Capability
    pcap    =	ints(1, 5), # Programmer Capability
    pcon    =	ints(1, 5), # Personnel Continuity
    aexp    =	ints(1, 5), # Applications Experience
    plex    =	ints(1, 5), # Platform Experience
    ltex    =	ints(1, 5), # Language and Tool Experience
    # Project Factors:
    tool    =	ints(1, 5), # Use of Software Tools
    site    =	ints(1, 6), # Multi-site Development
    sced    =	ints(1, 5), # Schedule pressure
    # defect removal methods
    automated_analysis  =	 ints(1, 6),
    peer_reviews  =	 ints(1, 6),
    execution_testing_and_tools  =	 ints(1, 6))

def perturb():
  def eq1(x,m,n):   return (x-3)*reals(m,n)+1 
  def eq2(x,m,n):   return (x-6)*reals(m,n) 
  def pem(a=1,b=5): return eq1(ints(a,b),  0.073,  0.21)
  def nem(a=1,b=5): return eq1(ints(a,b), -0.187, -0.078)
  def sf():         return eq2(ints(1,6), -1.58,  -1.014)

def cocomo2000(i):
  """Estimate calculates the quotient result from 
  dividing the person-month calculation, pm(), 
  by the amount of calendar time necessary to 
  develop the product, tdev()."""

  def size():
    """Size displays the overall size of the product.
    It is calculated from  the percentage of code 
    discarded, revl(), the new source lines of code,
    newsKsloc(), and the calculation of code reuse, 
    equivalentKsloc().
    """
    return (1+( i.revl /100)) \
        * (i.newKsloc+equivalentKsloc())

  def equivalentKsloc():
    """EquivalenKsloc is the calculation of code reuse.  It is derived from the
    size of the adapted component in thousands of adapted source lines of code,
    adaptedKsloc(), the adaptation adjustment modifier, aam(), and the
    percentage of code that is reengineered by automatic translation, at()."""
    return i.adaptedKsloc*aam()*(1-(i.at/100))

  def aam():
    """#aam is the adaptation adjustment modifier that returns the result of one
    of two calculations based on the value of the adaptation adjustment factor,
    aaf().  aam is calculated from the degree of assessment and assimilation,
    aa(), the adaptation adjustment factor, aaf(), the software understanding,
    su(), and the programmer's unfamiliarity with the software, unfm(). 
    This function was changed from the original version that contained errors.
    """
    f = aaf()
    if f <= 50 :
      return (i.aa+f*(1+(0.02*i.su*i.unfm)))/100
    else :      
      return (i.aa+f*(i.su*i.unfm))/100

  def aaf():
    """aaf is the adaptation adjustment factor and is calculated using the percentage
    of the adapted software's design that is modified, dm(), the percentage of
    code modified, cm(), and the percentage of effort necessary to integrate
    the reused software, im(). """
    return 0.4*i.dm+0.3*i.cm+0.3*i.im

  def  scedPercent():
    """scedPercent is the compression/expansion percentage in the sced effort
    multiplier rating scale. These values reflect the rating scale from
    Table 2.34, page 51 of the handout.  This function was added to the original 
    version. """ 
    y=i.sced
    return [75,85,100,130,160][int(round(y))]

  def tdev():
    """tdev is the amount of calendar time necessary to develop the product. It
    is calculated using the constant c(), the amount of effort in person-months, 
    pmNs(), the exponent used in the tdev function, f(), and the compression/
    expansion percentage in the sced effort multiplier rating scale,  
    scedPercent(). """
    return (i.c*(pmNs()**f()))*scedPercent()/100

  def f():
    """f is the exponent used in the tdev function.  It is calculated from the
    constants d and b, and the scale exponent used in the pmNs function.  """
    return i.d+0.2*(e()-i.b)

  def pm():
    """pm is the person-month calculation, the amount of time one person spends 
    working on the project for one month.  It is calculated from the amount
    of effort in person-months, pmNs(), the measure of the schedule constraint 
    for the project, sced(), and the estimated effort, pmAuto()."""
    return pmNs()*i.sced+pmAuto()

  def pmNs():
    """ pmNs is the amount of effort in person-months. pmNs is calculated from the 
    constant a(), size(), and the scale exponent, e(), and the following values. """
    return i.a * (size()**e()) *                               \
                     i.rely * i.data * i.cplx * i.ruse *        \
                     i.docu * i.time * i.stor * i.pvol *         \
                     i.acap * i.pcap * i.pcon * i.aexp * i.plex * \
                     i.ltex * i.tool * i.site    

  def pmAuto() :
   return (i.adaptedKsloc*(i.at/100))/i.atKProd 

  def e() :
    """e is the scale exponent used in the pmNs function.  It calculated from
    the constant b and the percent result of summing the selected scale
    scale factors"""
    return i.b+0.01*(i.Prec + i.Flex + i.Resl + i.Team + i.Pmat)

  #--- main
  months = pm()
  timE   = tdev()
  staff  = months/timE
  return months,timE,staff

def ground():
  "JPL ground systems"
  return dict(
    newKsloc = reals(11,392),
    adaptedtKsloc=0,
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
    adaptedtKsloc=0,
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
    adaptedtKsloc=0,
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
    adaptedtKsloc=0,
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
  acap=[5],pcap=[5],pcon=[5], aexp=[5], pexp=[5], ltex=[5])

def improveToolsTechniquesPlatform(): return dict(
  time=[3],stor=[3],pvol=[2],tool=[5], site=[6])

def improvePrecendentnessDevelopmentFlexibility(): return dict(
  Prec=[5],Flex=[5])

def increaseArchitecturalAnalysisRiskResolution(): return dict(
  Resl=[5])

def relaxSchedule(): return dict(
  sced = [5])

def improveProcessMaturity(): return dict(
  Pmat = [5])

def reduceFunctionality(): return dict(
  data = [2], nkloc=[0.5]) # nloc is a special symbol. Used to change kloc.

def improveTeam(): return dict(
  Team = [5])

def reduceQuality():  return dict(
  rely = [1], docu=[1], time = [3], cplx = [1])

rx = [ doNothing, improvePersonnel, improveToolsTechniquesPlatform,
       improvePrecendentnessDevelopmentFlexibility,
       increaseArchitecturalAnalysisRiskResolution, relaxSchedule,
       improveProcessMaturity, reduceFunctionality, improveTeam,
       reduceQuality]

def fill(f, rx=doNothing):
  def eq1(x,m,n):     return (x-3)*reals(m,n)+1 
  def eq2(x,m,n):     return (x-6)*reals(m,n) 
  def pem(x,a=1,b=5): assert a <= x <= b; return eq1(x,  0.073,  0.21)
  def nem(x,a=1,b=5): assert a <= x <= b; return eq1(x, -0.178, -0.078)
  def sf(x):          assert 1 <= x <= 6; return eq2(x, -1.6,   -1.014)
  i = o(**(base() | f() | rx()))
  tunings= dict(
            Prec=sf(i.Prec),       Flex=sf(i.Flex),      Resl=sf(i.Resl),  Team=sf(i.Team),   
            Pmat=sf(i.Pmat),      rely=pem(i.rely),     data=pem(i.data,2,5), 
            cplx=pem(i.cplx,1,6), ruse=pem(i.ruse,2,6), docu=pem(i.docu),    
            time=pem(i.time,3,6), stor=pem(i.stor,3,6), pvol=pem(i.pvol,2,5),
            acap=nem(i.acap),     pcap=nem(i.pcap),     pcon=nem(i.pcon),    
            aexp=nem(i.aexp),     plex=nem(i.plex),     ltex=nem(i.ltex),    
            tool=nem(i.tool),     site=nem(i.site,1,6), sced=nem(i.sced) )
  print(tunings)
  return o(**(i.__dict__ | tunings))

if __name__ == "__main__":
  parser = argparse.ArgumentParser(description='Cocomo simulator')
  parser.add_argument('-s', '--seed',    type=int, default=1234567891, help='random number seed (default: 1234567891)')
  parser.add_argument('-r', '--repeats', type=int, default=1,          help='number of reoeats (default: 1)')
  the = parser.parse_args()
  print(the)
  random.seed(the.seed)
  for _ in range(the.repeats):
    print(cocomo2000(fill(flight)))
