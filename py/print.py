
from contextlib import contextmanager

@contextmanager
def silence(epilog=False):
    global print
    old = print
    print = keeper
    yield
    print= old
    if epilog: keeper()

def something(a):
  print("I am something.",a)
  somethingElse(a+1)

def somethingElse(x):
   print("I am something else.", x)

def keeper(*l,kept=[]):
  if not l: 
    for n,x in enumerate(kept):
      print('log event',n,'is:',x)
  else:
    s = ', '.join([str(x) for x in l]) 
    kept += [s]

with silence(): something(10)  # nothing prints
with silence(epilog = True): something(10) # all prints are saved to end

print(22) # and, btw, print is back to its old self when we are done.
  
