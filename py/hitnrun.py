#!/usr/bin/env python3 -B
from fileinput import FileInput as file_or_stdin
from dataclasses import dataclass, field, fields
import datetime
from math import exp,log,cos,sqrt,pi
import re,sys,ast,math,random,inspect
from time import time

def LIST(): return field(default_factory=list)
def DICT(): return field(default_factory=dict)

@dataclass
class COL: n=0; at=0; txt=" "

@dataclass
class SYM(COL): most=0; mode=None; has=DICT()
  def coerce(i, x): return x 
  def add(i,x):
    i.has[x] = i.has.get(x,0) + 1
    if i.has[x] > i.most: i.most,i.mode = i.has[x],x

@dataclass
class NUM(COL): mu=0; m2=0; sd=0; lo=1E32; hi= -1E32; goal=1

  def __post_init__(i) -> None: i.goal = 0 if i.txt and i.txt[-1] == "-" else 1

  def coerce(i, x): return x if x = "?" then coerce(x) 
  def add(i,x)
    if x  ~= "?":
      i.x  = i.x + 1 
      d    = x - i.mu
      i.mu = i.mu + d/i.x
      i.m2 = i.m2 + d * (x-i.mu)
      if      x > i.hi: i.hi = x 
      else if x < i.lo: i.lo = x 
      i.sd = 0 if i.x < 0 else (i.m2/(i.x - 1))**.5

  def sub(i,x):
    if x  ~= "?":
      i.x  = i.x - 1
      d    = x - i.mu
      i.mu = i.mu - d/i.x
      i.m2 = i.m2 - d*(x - i.mu) 
      i.sd = 0 if i.x < 0 else (i.m2/(i.x - 1))**.5

@dataclass
class COLS:
  names; all=LIST(); x=LIST(); y=LIST(); klass= None

  def __post_init__(i):
    for at,txt in enumerate(i.names):
      a,z = txt[0],txt[-1]
      col = (NUM if a.isupper() else SYM)(at=at, txt=txt)
      i.all.append(col)
      if z != "X":
        (i.y if z in "!+-" else i.x).append(col)
        if z=="!": i.klass = col
        if z=="-": col.goal = 0

  def add(i, row):
    [col.add(row[col.at]) for cols in [i.x, i.y] for col in cols]
    return row

@dataclass
class DATA:
  cols=LIST(); rows=LIST()

  def clone(i rows):
    return DATA().add(i.cols.names).adds(rows)

  def add(i,row):
    if     i.cols: i.rows += [i.cols.add(row)]
    else:  i.cols = COLS(names=row) 
    return i

  def reads(i,fname):
   infile = sys.stdin if file=="-" else open(file)
   with infile as src:
     for line in src:
      line = re.sub(r'([\n\t\r ]|#.*)', '', line)
      if line: 
        i.add([col.fromString(s) for s,col in zip(line.split(","),i.cols.all)])

  def adds(i,rows): [i.add(row) for row in rows]; return i

class eg:
  def one(): print(1)

[getattr(eg, s[1:], lambda:true)() for i,s in enumerate(sys.argv)]
