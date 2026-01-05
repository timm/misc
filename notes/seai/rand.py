
import random, sys, xai
xai.the.data=sys.argv[2]
random.seed(int(sys.argv[1]))

data = xai.Data(xai.csv(xai.the.data))

def Y(r): return round(xai.disty(data,r),2)

def report(rows): 
    a=sorted(rows[:],key=Y); print( len(a),"\t",Y(a[0]),Y(a[len(a)//2]))

report(data.rows)
report(xai.shuffle(data.rows)[:30])
