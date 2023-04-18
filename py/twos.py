import math

def solve(m1,m2,std1,std2):
  a = 1/(2*std1**2) - 1/(2*std2**2)
  if a==0: return (m1+m2)/2
  b = m2/(std2**2) - m1/(std1**2)
  c = m1**2 /(2*std1**2) - m2**2 / (2*std2**2) - math.log(std2/std1)
  fun = lambda n: (-b + n*math.sqrt(b**2 - 4*a*c))/(2*a)
  return fun(1) if m1 <= fun(1) <= m2 else fun(-1)

print( solve(2.5,5,1,1))
print( solve(5,10,0.99,1.01))
