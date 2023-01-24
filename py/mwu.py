
data =dict(placebo=[7, 5, 6, 4, 12],
           drug=[3, 6, 4, 2,1])
           
class o:
  "Return a class can print itself (hiding 'private' keys)"
  def __init__(i, **d)  : i.__dict__.update(d)
  def __repr__(i) : return "{"+ ', '.join(
    [f":{k} {v}" for k, v in i.__dict__.items() if  k[0] != "_"])+"}"
    
def mwu(d,k1,k2):
	lst2=  [o(left=0,right=0,here=1,x=x) for x in d[k1]]
	#ls2 +=  [o(left=0,right=0,here=1,y=y) for y in d[k2]]
	print(lst2)
     

mwu(data)
