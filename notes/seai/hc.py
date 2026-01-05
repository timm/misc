import random, sys, xai
xai.the.data=sys.argv[2]
random.seed(int(sys.argv[1]))

data = xai.Data(xai.csv(xai.the.data))

def Y(r): return round(xai.disty(data,r),2)

def report(rows): 
    a=sorted(rows[:],key=Y); print( len(a),"\t",Y(a[0]),Y(a[len(a)//2]))

labelled, rows = [], data.rows
report(rows)
while len(labelled) < 30:
  labelled = labelled + xai.shuffle(rows)[:10]
  best = min(labelled,key=Y)
  X=lambda r:xai.distx(data,r,best)
  rows = sorted(rows,key=X)[:len(rows)//2]
  report(labelled)

