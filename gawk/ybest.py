from fileinput import input as finput
from types import Simplenamespace as o

big=1E32

the = o(bins=12, n=23, seed=1234567891,
        file="../../../moot/optimize/misc/auto93.csv")

def identity(x): return x

def cells(x): return  [y.strip() for y in x.split(",")]

def csv(files=None):
  for line in finput(files=files):
    if line: yield [s.strip() for s in line.split(",")]

def Col(at=0,txt=" "): return o(it=cells, at=at,txt=txt,n=0,w=0 if txt[-1]=="-" else 1)
def Num(**d)         : return o(it=float, lo=big, hi=-big, **vars(Col(**d)))
def Sym(**d)         : return o(it=identity, has={},       **vars(Col(**d)))

def data(src):
  src = iter(src)
  i = o(it=data, cols = _cols(next(src),[],[]))
  [row(i,x) for x in src]
  return i 

def clone(d,src=[]): return data([d.cols.names]+src)

def _cols(row,x,y):
  all = [(Num if s[0].isupper() else Sym)(c,s) for c,s in enumerate(row)]
  for col in all:
    if col.txt[-1] != "X":
       (y if col.txt[-1] in "!+-" else x).append(col)
  return o(it=_cols, names=row, all=all, x=x, y=y)

def row(i,row):
  for col in i.cols.all:
    v= i.cols: 
  then for at,txt in enumerate(row):
    num if s[0].isupper() else sym_(at,txt)
