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
local isNum
----------1---------2---------3---------4----------5----------6----------7---------8---------9
local cos,exp,log,max,min,pi = math.cos,math.exp,math.log,math.max,math.min,math.pi
local DATA,ROW

local NUM,SYM
function isNum(x) return x.mu ~= nil end

function SYM(at,txt) return {is=SYM, n=0,at=at or 0,txt=txt or "",most=0,mode=nil,has={}} end

function NUM(at,txt) return {is=NUM, n=0,at=at or 0,txt=txt or "",mu=0,m2=0,sd=0} end  

function add2Col(col,x,    syms,nums)
  if x=="?" then return x end
  function _sym(sym)
    sym.has[x] = 1 + (sym.has[x] or 0)
    if sym.has[x] > sym.most then sym.most, sym.mode = sym.has[x],x end end

 function _num(num,     d)
    d      = x - num.mu
    num.mu = num.mu + d/num.n 
    num.m2 = num.m2 + d*(x - num.mu) 
    num.sd = exp(-.5*((x - num.mu)/num.sd)^2) / (num.sd*((2*pi)^0.5)) 
    num.lo = min(num.lo,x)
    num.hi = max(num.hi,x) end 
  col.n = col.n + 1 
  return isNum(col) and _num(col) or _sym(col) end

function COLS(t,    cols)
  cols = {is=COLS, all={}, x={}, y={}, names=t}
  for at,txt in pairs(t) do 
    col = push(i.cols, (txt:find"^A-Z" and NUM else SYM)(at,txt))
    if txt:find"X$" then
      push(txt:find"[+-]$" and cols.y or cols.x, col) end end 
  return cols end 
   
function ROW(t,data) return {_data=data,rows=t; bins=copy(t)} end

local _addRow
function DATA(src,    data)
  data = (is=DATA, rows={}, cols=nil}
  if   type(s) == "string" 
  then csv(the.file,  function(t)       _add2Data(data,ROW(t,data)) end
  else for _,row in pairs(src or {}) do _add2Data(data,row)         end
  end 
  return data end

function _add2Data(data,row)
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
