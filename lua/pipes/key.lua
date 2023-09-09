#!/usr/bin/env lua
l=require"lib"
local the=l.settings[[

bins: discretize

USAGE: ./bins

OPTIONS
  -b --bins number of bins = 5
  -f --file file to load   = 
  -h --help show help      = false]]

local box,csv,kap,oo,push,sort = l.box,l.csv,l.kap,l.oo,l.push,l.sort
----------1---------2---------3---------4----------5----------6----------7---------8---------9
local cos,exp,log,max,min,pi = math.cos,math.exp,math.log,math.max,math.min,math.pi
local DATA,ROW

local NUM,SYM

function SYM(at,txt) return {is=SYM, n=0,at=at or 0,txt=txt or "",most=0,mode=nil,has={}} end
function NUM(at,txt) return {is=NUM, n=0,at=at or 0,txt=txt or "",mu=0,m2=0,sd=0} end  

local function add2Col(col,x,    d)
  if x=="?" then return x end
  col.n = col.n + 1 
  if   col.is == SYM 
  then col.has[x] = 1 + (col.has[x] or 0)
       if col.has[x] > col.most then col.most, col.mode = col.has[x],x end 
  else d      = x - col.mu
       col.mu = col.mu + d/col.n 
       col.m2 = col.m2 + d*(x - col.mu) 
       col.sd = exp(-.5*((x - col.mu)/col.sd)^2) / (col.sd*((2*pi)^0.5)) 
       col.lo = min(col.lo,x)
       col.hi = max(col.hi,x) end end

local _bins={
     3={ −0.43,	 0.43},
     4={ −0.67, 	 0,	     0.67},
     5={ −0.84,	−0.25,	 0.25, 	 0.84},
     6={ −0.97,	−0.43,	 0,	     0.43,	0.97},
     7={ −1.07,	−0.57,	−0.18,	 0.18,	0.57,	1.07},
     8={ −1.15,	−0.67,	−0.32, 	 0,	    0.32,	0.67,	1.15},
     9={ −1.22,	−0.76,	−0.43,	−0.14,	0.14,	0.43,	0.76,	1.22},
    10={ −1.28,	−0.84,	−0.52,	−0.25,	0,	  0.25,	0.52,	0.84,	1.28}}

local function bin(col,x,     tmp)
  if x=="?"      then return x end
  if col.is==SYM then return x end 
  tmp = (x- col.mu)/col.sd
  for b,x in pairs(breaks[the.bin])  do if tmp <= x then return b end end
  return the.bins end

function COLS(t,    cols)
  cols = {is=COLS, all={}, x={}, y={}, names=t}
  for at,txt in pairs(t) do 
    col = push(i.cols, (txt:find"^A-Z" and NUM or SYM)(at,txt))
    if txt:find"X$" then
      push(txt:find"[+-]$" and cols.y or cols.x, col) end end 
  return cols end 
   
function ROW(t,data) return {_data=data,rows=t; bins=copy(t)} end

function DATA(src,    data,add1)
  function _add1(row)
    if   data.cols 
    then push(data.rows, row)
         for _,cols in pairs{data.cols.x, data.cols.y} do 
           for _,col in pairs(cols) do 
             add(col, row.cells[col.at]) end end 
    else data.cols = cols(row.cells) end end 
  -----------------------------------
  data = (is=DATA, rows={}, cols=nil}
  if   type(s) == "string" 
  then csv(the.file,  function(t) _add1(data,ROW(t,data)) end) 
       _discretize(data)
  else for _,row in pairs(src or {}) do _add1(data,row) end       
  return data end

function _discretize(data)
  for _,col in pairs(data.cols.all) do 
    for _,row in pairs(data,rows) do 
      row.bins[col.at] = bin(col, row.cells[col.at]) end end end end end 

local function dist(row1,row2)
  d=0; for _,col in pairs(row1._data.cols.x) do d=d+_dist1(col, row1.dist[col.at], row2.dist[col.at]]) end--------------------------------

the = l.cli(the)
print(table.concat(row,", "))
for _,row  in pairs(rows) do
  print(table.concat(row,", "))
  print(table.concat(map(row,bin),", ")) end
