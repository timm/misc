# p =1/sqrt(n)
# x= \mu + \simg*invCDG(p)
# ilast terms gives the z-score where 1/√n of the data is below.
#
# so invCDF(p_ = sqrt(2) *  erfinv(2p-1)
# and  efInv is the winitzki approximation
#

import math
def cdf(x, mu=0, sd=1):
  _cdf = lambda z: 1 - 0.5 * math.exp(-0.717*z - 0.416*z*z)
  z = (x - mu) / sd
  return _cdf(z) if z >= 0 else 1 - _cdf(-z)

import random
def one():
    mu= random.uniform(0,1)
    sd = random.uniform(0,.2)
    n  = random.uniform(5,100)
    r  = lambda  x: round(x,2)
    p  = 1 / n**0.5
    lo = mu-3*sd
    hi = mu+3*sd
    x  = random.uniform(lo,hi)
    q  = (x - lo)/(hi - lo + 1E-32)
    if abs((c:=cdf(x,mu,sd)) - p)<0.1 : return [r(x) for x in [n,lo,x,hi]]

for _ in range(100):
  if (z := one()): print(z)
