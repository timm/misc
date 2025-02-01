import math

def triXcuts(a=0, b=0.5, c=1, n=10):
  def inverse(p):
    return a+(p*(b-a)*(c-a))**.5 if p<=(b-a)/(c-a) else c-((1-p)*(c-a)*(c-b))**.5
  cdf_values = [(i / n) for i in range(1, n)]
  return [inverse(p) for p in cdf_values] + [c]

# Example usage
# print(triXcuts(n=7))
# print(triXcuts(a=1,b=5,c=6,n=10))
#
def triCdf(x, a=0, b=0.5, c=1):
    if   x < a: return 0.0
    elif x < b: return ((x - a) ** 2) / ((b - a) * (c - a))
    elif x < c: return 1 - ((c - x) ** 2) / ((c - a) * (c - b))
    else:       return 1.0

# print(triCdf(.2))
# print(triCdf(.4))
# print(triCdf(.5))
# print(triCdf(.6))
# print(triCdf(.8))

def linCdf(x,mu,sd):
    z = (x-mu)/sd
    return  1 - 0.5 * math.exp(-0.72 * z - 0.42 * z * z)

import math

def xlin_phi(x, mu=0, sigma=1):
    """
    Linear approximation to the Gaussian cumulative distribution function (CDF).

    Parameters:
    x (float): The value for which to calculate the CDF.
    mu (float): The mean of the Gaussian distribution. Default is 0.
    sigma (float): The standard deviation of the Gaussian distribution. Default is 1.

    Returns:
    float: The approximate value of the Gaussian CDF for the given x.
    """
    if sigma <= 0:
        raise ValueError("Standard deviation must be positive")

    z = (x - mu) / sigma

    if z >= 0:
        return 1 - 0.5 * math.exp(-0.72 * z - 0.42 * z * z)
    else:
        return 0.5 * math.exp(0.72 * z + 0.42 * z * z)

def lin_phi(x, mu=0, sigma=1):
    z = (x - mu) / sigma
    if z >= 0:
        return 1 - 0.5 * math.exp(-0.72 * z - 0.42 * z * z)
    else:
        return 0.5 * math.exp(0.72 * abs(z) + 0.42 * z * z)

# Example usage
for x in range(20):
    print(f"x = {x-10:3}     cdf = {round(lin_phi(x-10, mu=0,sigma=3),3)}")
