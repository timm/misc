#!/usr/bin/env python3 -B
# <!-- vim: set ts=2 sw=2 sts=2 et: -->
import sys,random,argparse

class o:
  def __init__(i,**d): i.__dict__.update(d)
  def __repr__(i): return i.__class__.__name__ + str(i.__dict__)
 
def of(x):     return random.choice(x)
def ints(a,b): return random.randint(a,b)
def reals(a,b): return random.uniform(a,b)

#-------------------------------------------------------
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
    reduceLoc = 1,   # reduce knlow by this facor
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
    automated_analysis           =	 ints(1, 6),
    peer_reviews                 =	 ints(1, 6),
    execution_testing_and_tools  =	 ints(1, 6))

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
    return (1+( i.revl /100)) * i.reduceLoc * (i.newKsloc+equivalentKsloc())

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

def fill(f, rx=None):
  def eq1(x,m,n):     return (x-3)*reals(m,n)+1 
  def eq2(x,m,n):     return (x-6)*reals(m,n) 
  def pem(x,a=1,b=5): assert a <= x <= b; return eq1(x,  0.073,  0.21)
  def nem(x,a=1,b=5): assert a <= x <= b; return eq1(x, -0.178, -0.078)
  def sf(x):          assert 1 <= x <= 6; return eq2(x, -1.6,   -1.014)
  d = (base() | f()  | rx()) if rx else (base() | f())
  i = o(**d)
  tunings = dict(
            Prec=sf(i.Prec),      Flex=sf(i.Flex),      Resl=sf(i.Resl),  Team=sf(i.Team),   
            Pmat=sf(i.Pmat),      rely=pem(i.rely),     data=pem(i.data,2,5), 
            cplx=pem(i.cplx,1,6), ruse=pem(i.ruse,2,6), docu=pem(i.docu),    
            time=pem(i.time,3,6), stor=pem(i.stor,3,6), pvol=pem(i.pvol,2,5),
            acap=nem(i.acap),     pcap=nem(i.pcap),     pcon=nem(i.pcon),    
            aexp=nem(i.aexp),     plex=nem(i.plex),     ltex=nem(i.ltex),    
            tool=nem(i.tool),     site=nem(i.site,1,6), sced=nem(i.sced) )
  return o(**(d | tunings))

def settings():
  parser = argparse.ArgumentParser(description='Cocomo simulator')
  parser.add_argument('-s', '--seed',    type=int, default=1234567891, help='random number seed (default: 1234567891)')
  parser.add_argument('-r', '--repeats', type=int, default=1,          help='number of reoeats (default: 1)')
  return  parser.parse_args()

