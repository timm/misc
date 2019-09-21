#!/usr/bin/env python3
# vim : nospell filetype=py ts=2 sw=2 sts=2  et  :

import random,sys,re,os
from the import *

# -------------------------------------------------
# standard shortcuts

r    = random.random
seed = random.seed
isa  = isinstance
def first(l): return l[0]
def last(l):  return l[-1]
def isNum(x): return isa(x,(float,int))
def same(x):  return x

# -------------------------------------------------
# string stuff

def atom(x):
  "coerce x into the right kind of atom"
  try: return int(x)
  except:
    try: return float(x)
    except: return x

# -------------------------------------------------
# some convenient iterators

def words(f):
  with open(f) as fp:
    for line in fp:
      for word in line.strip().split():
        yield word

def string(s):
  "read lines from a string"
  for line in s.splitlines():
    yield line

def file(f):
  "read lines from a file"
  with open(f) as fp:
    for line in fp:
      yield line.strip()

# -------------------------------------------------
# error handling

def now(t,m):
  "maybe complain and exit"
  if not t:
    sys.stderr.write('#E> '+str(m)+'\n')
    sys.exit()

# -------------------------------------------------
# cli tricks

def cli():
  "Allow command lines args to update fields in the nested THE" 
  args   = [thing(x) for x in sys.argv[1:]]
  what   = {}
  groups = THE.d()
  while args:
    arg = args.pop(0)
    if arg in groups:
      what = groups[arg].d()
    else:
      now(isa(arg,str) and arg[0] == "-", "bad flag '%s'" %arg)
      arg = arg[1:]
      now(arg in what, "%s not one of %s" % (arg,list(what.keys())))
      old = what[arg]
      if isa(old, bool):
        what[arg] = not what[arg]
      else:
        val = args.pop(0)
        now(type(old) == type(val), 
             "'%s' value not of type '%s'" % (arg,type(old)))
        what[arg] = val
  return THE   

