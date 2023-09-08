#!/usr/bin/env lua
l=require"lib"
local the=l.settings[[

bins: discretize

USAGE: ./bins

OPTIONS
  -b --bins number of bins = 5
  -f --file file to load   = 
  -h --help show help      = false]]

local box,csv,kap,oo,sort = l.box,l.csv,l.kap,l.oo,l.sort
local isNum
----------1---------2---------3---------4----------5----------6----------7---------8---------9
local cos,exp,log,max,min,pi = math.cos,math.exp math.log,math.max,math.min,math.pi
local DATA,ROW

local NUM,SYM
function isNum(x) return x.mu ~= nil end
function SYM(at,txt)
  return {n=0,at=at or 0,txt=txt or "",most=0,mode=nil,has={}} end

function NUM(at,txt)
  return {n=0,at=at or 0,txt=txt or "",mu=0,m2=0,sd=0} end

function add(col,x,    sym,num)
  if x=="?" then return x end
  function sym(i)
    i.has[x] = 1 + (i.has[x] or 0)
    if i.has[x] > i.most then i.most, i.mode = i.has[x],x end end 
  function num(i,     d)
    d    = x - i.mu
    i.mu = i.mu + d/i.n 
    i.m2 = i.m2 + d*(x - i.mu) 
    i.sd = exp(-.5*((x - i.mu)/i.sd)^2) / (i.sd*((2*pi)^0.5)) 
    i.lo = min(i.lo,x)
    i.hi = max(i.hi,x) end 
  col.n = col.n + 1 
  return isNum(col) and num(col) or sym(col) end 

function COLS(t,    cols)
  cols = {all={}, x={}, y={}, names=t}
  for at,txt in pairs(t) do 
    col = push(i.cols, (txt:find"^A-Z" and NUM else SYM)(at,txt))
    if txt:find"X$" then
      push(txt:find"[+-]$" and cols.y or cols.x, col) end end 
  return cols end 
   
function ROW(t,data) return {_data=data,rows=t; bins=copy(t)} end

local _addRow
function DATA(src,    data)
  data = (rows={}, cols=nil}
  if   type(s) == "string" 
  then csv(the.file,  function(t)       _addRow(data,ROW(t,data)) end
  else for _,row in pairs(src or {}) do _addRow(data,row)         end
  end 
  return data end

function _addRow(data,row)
  if data.cols then 
     push(data.rows, row)
     for _,cols in pairs{data.cols.x, data.cols.y} do 
        for _,col in pairs(cols) do 
          add(col, row.cells[colat]) end end 
  else 
    data.cols = cols(row.cells) end end

------------------
the = l.cli(the)
print(table.concat(row,", "))
for _,row  in pairs(rows) do
  print(table.concat(row,", "))
  print(table.concat(map(row,bin),", ")) end
