import datetime
from contextlib import contextmanager

def slowfib(n):
    return n if n<2 else slowfib(n-1) + slowfib(n-2)

def fastfib(n):
    a, b = 0, 1
    for i in range(0, n):
        a, b = b, a + b
    return a

def memo(f):
    b4 = {}
    def helper(x):
        if x not in b4:            
            b4[x] = f(x)
        return b4[x]
    return helper
 
@memo
def fib(n):
    return n if n<2 else fib(n-1) + fib(n-2)

@contextmanager
def watch():
  a = datetime.datetime.now()
  yield
  b = datetime.datetime.now()
  print("time= %s micros" % (b-a).microseconds)

for i in range(1,100):
  print("")
  with watch(): print("slowfib",i,slowfib(i),end=" ")
  with watch(): print("fastfib",i,fastfib(i),end=" ")
  with watch(): print("memofib",i,fib(i),end=" ")


