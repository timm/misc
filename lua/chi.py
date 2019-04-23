def interpolate(x,xy): 
  x1,y1 = xy[0]
  if x < x1: return y1
  for x2,y2 in xy[1:]:
    if x1 <= x < x2:
      return y1 + (y2-y1) * (x - x1) / (x2 - x1)
    x1,y1 = x2,y2
  return y2

def countsequal(obs,xpect):
  x2,df = 0,-1
  for k in xpect:
    df += 1
    e   = xpect[k]
    o   = obs.get(k,0)
    x2 += (o  - e)**2/ e
  critical = interpolate(df,[ # 95% confidence
                ( 1,  3.841), ( 5, 11.070), 
                (10, 18.307), (15, 24.996), (20, 31.410),
                (30, 43.66),  (60, 79.08)])
  #print(dict(df=df,x2=x2,critical=critical))
  return x2 <= critical

if __name__== "__main__":
  d1 = dict(a=20,b=10,c=60,d=80,e=100,f=10,g=20)
  x,y=0,0.025
  while x < 2:
    y *= 1.5
    x  = 1+y
    d2 = {k:v*x for k,v in d1.items()}
    print(x, countsequal(d2,d1))
  
