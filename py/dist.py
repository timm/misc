
 def symDist(a,b,c):
   return 0 if a==b else 1

 def numDist(a,b,c):
   if    a is ignore: b = norm(b,c); a = 0 if b>0.5 else 1
   elif  b is ignore: a = norm(a,c); b = 0 if a>0.5 else 1
   else: a,b = norm(a,c), norm(b,c)
   return (a-b)**d

 def dist(a,b):
   m,n = 0, 0.00000001
   for c,(a1,b1) in enumerate(zip(a,b)):
     if not (a1 is ignore and b1 is ignore):
       f  = numDist if nump(a) or nump(b) else symDist
       m += f(a1, b1, c)
       n += 1
   return (m/n)**(1/d)

