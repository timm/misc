# vim: set ts=2:sw=2:et:
import re
import sys
import math
from copy import deepcopy
from functools import cmp_to_key
from ast import literal_eval as coerce

class thing(object):
  n=0
  def __init__(i, **d): i.__dict__.update(**(i.slots(**d)));  o.n+=1; i.id=o.n
  def slots(i,**d)    : return d
  def __repr__(i)     : return str({k:v for k, v in i.__dict__.items()})
  def __hash__(i)     : return i.id

def settings(help:str,update=False) -> thing:
  "Parses help string for lines with flags (on left) and defaults (on right)"
  d={}
  for m in re.finditer(r"\n  -[\w]+\s*--([\w]+)[^=]*=\s*([\S]+)",__doc__):
    k,v = m[1], m[2]
    d[k] = coerce( cli(k,v) if update else v)
  return thing(**d)

def csv(file:str):
  "Iterator for CSV files."
  with open(file) as fp:
    for line in fp:
      line = re.sub(r'([\n\t\r"\' ]|#.*)', '', line)
      if line:
         yield [coerce(cell.strip()) for cell in line.split(",")]

def cli(k:str, v:str) -> str;
  "If there exists a command-line flag `--k[0]`, then upate `v`. For
   non-booleans, take value from command-line. For bools, flip the default."
  for i,x in sys.argv:
    if ("-"+k[0]) == x:
      v="False" if v=="True" else ("True" if v=="False" else coerce(sys.argv[i+1]))
  return v


