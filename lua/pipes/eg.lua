#!/usr/bin/env lua
--                       ___                        
--                      /\_ \                       
--    __     __         \//\ \    __  __     __     
--  /'__`\ /'_ `\         \ \ \  /\ \/\ \  /'__`\   
-- /\  __//\ \L\ \      __ \_\ \_\ \ \_\ \/\ \L\.\_ 
-- \ \____\ \____ \    /\_\/\____\\ \____/\ \__/.\_\
--  \/____/\/___L\ \   \/_/\/____/ \/___/  \/__/\/_/
--           /\____/                                
--           \_/__/                                 

local the = require("lib").settings.create[[

eg: demonstrator of "less is more"
(c) 2023, Tim Menzies, <timm@ieee.org>, BSD-2

USAGE: 
  require"eg"
  or lua egs.lua [OPTIONS]

OPTION:
  -b --bins      number of bins         = 5
  -d --decimals  print first `decimals` = 2
  -f --file      csv file to load       = 
  -F --Far       distance to far        = .9
  -g --go        start up action        = nothing
  -h --help      show help              = false
  -H --Half      items explored in halving = 256]]
-------------------- ------------------- --------------------- -------------------- ----------
local l = require("lib")
local csv,kap,oo,push,sort   = l.str.csv, l.list.kap, l.str.oo, l.list.push, l.list.sort
local cos,exp,log,max,min,pi = math.cos,math.exp,math.log,math.max,math.min,math.pi

local DATA,ROW,NUM,SYM = obj"DATA", obj"ROW", obj"NUM", obj"SYM"
-------------------- ------------------- --------------------- -------------------- ----------
--  _    ._ _  
-- _> \/ | | | 
--    /        

function SYM:init(at,txt) 
  return {n=0,at=at or 0,txt=txt or "",most=0,mode=nil,has={}} end

function SYM:add(x,n)
  if x~="?" then 
    n = n or 1
    self.n = self.n + n 
    self.has[x] = n + (self.has[x] or 0)
    if self.has[x] > self.most then 
      self.most, self.mode = self.has[x],x end end end

function SYM:bin(x) return x end
function SYM:dist(x,y) return x==y and 0 or 1 end
-- -------------------- ------------------- --------------------- -------------------- ----------
-- ._      ._ _  
-- | | |_| | | | 

function NUM:init(at,txt) 
	return {n=0,at=at or 0,txt=txt or "",mu=0,m2=0,sd=0} end  

function NUM:add(x,    d)
  if x ~="?" then 
    self.n  = self.n + 1 
    d       = x - self.mu
    self.mu = self.mu + d/self.n 
    self.m2 = self.m2 + d*(x - self.mu) 
    self.sd = sqrt(self.m2/(i.n - 1))
    self.lo = min(self.lo,x)
    self.hi = max(self.hi,x) end end

function NUM:norm(x)
  return x=="?" and x or (x-self.lo)/(self.hi - self.lo + 1E-30) end

function NUM:dist(x,y) 
  if x=="?" and y=="?" then return 1 end
  x,y = self:norm(x), self:norm(y)
  x = x~="?" and x or (y<.5 and 1 or 0)
  y = y~="?" and y or (x<.5 and 1 or 0)
  return abs(x - y) 

function NUM:bin(x,     tmp)
  if x=="?"      then return x end
  tmp = (x - col.mu)/col.sd
  for b,x in pairs(breaks[the.bins])  do if tmp <= x then return b end end
  return the.bins end

NUM._bins= {
    [ 3] = { -.43,	 .43},
    [ 4] = { -.67,     0,	 .67},
    [ 5] = { -.84,  -.25,  .25,  .84},
    [ 6] = { -.97,	-.43,    0,	 .43,  .97},
    [ 7] = { -1.07,	-.57,	-.18,	 .18,  .57, 1.07},
    [ 8] = { -1.15,	-.67,	-.32, 	 0,	 .32,  .67, 1.15},
    [ 9] = { -1.22,	-.76,	-.43,	-.14,	 .14,	 .43,  .76,	1.22},
    [10] = { -1.28,	-.84,	-.52,	-.25,	   0,	 .25,  .52,	 .84,	1.28}}
-------------------- ------------------- --------------------- -------------------- ----------
--  _  _  |  _ 
-- (_ (_) | _> 

function COLS:init(t,    col,category)
  self.all, self.x, self.y, self.names = {},{},{},t
  for at,txt in pairs(t) do 
    col = txt:find"^[A-Z]" and NUM(at,txt) or SYM(at,txt)
    push(self.all, col)
    if txt:find"X$" then
      category = txt:find"[+-]$" and self.y or self.x
      push(category, col) end end end

function COLS:add(row)
  for _,cols in pairs{self.cols.x, self.cols.y} do for _,col in pairs(cols) do 
    col:add(row.cells[col.at]) end end
  return row end
-------------------- ------------------- --------------------- -------------------- ----------
-- ._  _       
-- |  (_) \/\/ 

function ROW:init(t,data) 
  return {_data=data,rows=t; bins=list.copy(t)} end

function ROW:dist(i,j)
  d,n = 0,0
  for _,col in pairs(row1._data.cols.x) do
    n = n + 1
    d = d + (col:dist(row1.dist[col.at], row2.dist[col.at]))^the.p end
  return (d/n)^(1/the.p) end

function ROW:neighbors(rows)
  return keysort(rows, function(row2) return self:dist(row2) end)

function ROW:extremities(rows,     n,x,y)
  n = (#rows*the.Far)//1
  x = self:neighbors(rows)[n]
  y = x:neighbors(rows)[n]
  return x,y, x:dist(y)
-------------------- ------------------- --------------------- -------------------- ----------
--  _|  _. _|_  _. 
-- (_| (_|  |_ (_| 

function DATA:init(src)
  self.rows={}
  if   type(s) == "string" 
  then csv(the.file,  function(t) self:add(ROW(t,self)) end) 
       self:bins()
  else for _,row in pairs(src or {}) do self:add(row) end end end

function DATA:clone(rows)
  data = DATA({self.cols.names})
  for row in pairs(rows or {}) do data:add(row) end `
  return data end

function DATA:add(row)
  if self.cols then push(self.rows, self.cols:add(row)) else self.cols=COLS(self.cells) end end 

function DATA:bins()
  for _,row in pairs(rows) do
    for _,cols in pairs{self.cols.x, self.cols.y} do for _,col in pairs(cols) do 
      row.bins[col.at] = col:bin(x) end end end end

function DATA:half(rows,sorted,     a,b,C,as,bs,some)
  some  = rand.many(rows or self.rows, min(the.Half, #rows))
  a,b,C = some[1]:extremities(some)
  as,bs = {},{}
  if sorting and b:better(a) then a,b = b,c end
  for n,row in pairs(sorted(rows, function(r) return (r:dist(a)^2+C^2-r:dist(b)^2)/(2*C) end)) do
    push(n <= #rows/2 and as or bs, row) end
  return a,b,as,bs

-------------------- ------------------- --------------------- -------------------- ----------
return {the=the, DATA=DATA, ROW=ROW, SYM=SYM, NUM=NUM, COLS=COLS}
