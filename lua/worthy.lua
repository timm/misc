class o:
     def __init__(i, **d): i.__dict__.update(d)
r=random.random
any=random.choice
more=1
less=-1

def weight()             : return any([more,less])
def num(lo=0, hi=1)      : return lo+r()*(hi - lo)
def sym(lst=[True,False]): return any(lst)
def some(n,one)          : return [one() for _ in range(n)]

def about():
return o(x= o( nums= lambda :some(5, num),
		syms= lambda: some(2, sym)), 
	y= o( nums= lambda :some(5, num),
	ws  = lambda some(3,weight)))

