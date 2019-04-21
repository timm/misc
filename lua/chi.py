def chisquare(d1,d2):
  "returns true if same"
  vals= [(1,  5.024), ( 3, 9.348), (  6, 14.449), (12,23.337),
         (25,40.646), (50,71.420), (100,129.561)]
  def threshold(df): 
     if df <= vals[ 0][0]: return vals[ 0][1]
     if df >= vals[-1][0]: return vals[-1][1]
     df1,x1 = vals[0]
     for df2,x2 in vals[1:]:
       if df1 <= df < df2:
         return x1 + (x2-x1) * (df - df1) / (df2-df1)
       df1,x1 = df2,x2
  chi,df = 0,-1
  for k,e in d1.items():
    df  += 1
    o    = d2.get(k,0)
    chi += (o  - e)**2/ e
  return chi < threshold(df)

d1=dict(a=20,b=10,c=60,d=80,e=100,f=10,g=20)

x,y=0,0.025
while x < 2:
  y *= 2
  x  = 1+y
  d2 = {k:v*x for k,v in d1.items()}
  print(x, chisquare(d1,d2))

