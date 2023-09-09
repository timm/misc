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

local l   = requre("etc")
local the = l.settings.create[[

eg: demonstrator of "less is more"
(c) 2023, Tim Menzies, <timm@ieee.org>, BSD-2

USAGE: 
  require"eg"
  or lua egs.lua [OPTIONS]

OPTION:
  -b --bins      number of bins         = 5
  -d --decimals  print first `decimals` = 2
  -f --file      csv file to load       = 
  -g --go        start up action        = nothing
  -h --help      show help              = false]]
-------------------- ------------------- --------------------- -------------------- ----------
local cos,exp,log,max,min,pi = math.cos,math.exp,math.log,math.max,math.min,math.pi
local csv,kap,oo,push,sort   = l.str.csv, l.list.kap, l.str.oo, l.list.push, l.list.sort

local DATA,ROW,NUM,SYM = obj"DATA", obj"ROW", obj"NUM", obj"SYM"
-------------------- ------------------- --------------------- -------------------- ----------
--  _    ._ _  
-- _> \/ | | | 
--    /        

function SYM:init(at,txt) 
  return {n=0,at=at or 0,txt=txt or "",most=0,mode=nil,has={}} end

function SYM:add(x)
  if x~="?" then 
    self.n = self.n + 1 
    self.has[x] = 1 + (self.has[x] or 0)
    if self.has[x] > self.most then 
      self.most, self.mode = self.has[x],x end end end

function SYM:bin(x) return x end
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
    self.sd = exp(-.5*((x - self.mu)/self.sd)^2) / (self.sd*((2*pi)^0.5)) 
    self.lo = min(self.lo,x)
    self.hi = max(self.hi,x) end end

function NUM:bin(x,     tmp)
  if x=="?"      then return x end
  tmp = math.floor((x - col.mu)/col.sd)
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

function COLS:init(t)
  self.all={}; self.x={}; self.y={}; self.names=t
  for at,txt in pairs(t) do 
    col = push(self.all, (txt:find"^A-Z" and NUM or SYM)(at,txt))
    if txt:find"X$" then
      push(txt:find"[+-]$" and self.y or self.x, col) end end end

function COLS:add(row)
  for _,cols in pairs{self.cols.x, self.cols.y} do for _,col in pairs(cols) do 
    col:add(row.cells[col.at]) end end
  return row end
-------------------- ------------------- --------------------- -------------------- ----------
-- ._  _       
-- |  (_) \/\/ 

function ROW:init(t,data) 
  return {_data=data,rows=t; bins=list.copy(t)} end

function ROW.dist(i,j)
   --- XXX over anutjomg
  d,n = 0,0
  for _,col in pairs(row1._data.cols.x) do 
    n = n+1
    d = d+ )_dist1(col, row1.dist[col.at], row2.dist[col.at]])^the.p end
  return (d/n)^(1/the,p)
-------------------- ------------------- --------------------- -------------------- ----------
--  _|  _. _|_  _. 
-- (_| (_|  |_ (_| 

function DATA:init(src)
  self.rows={}
  if   type(s) == "string" 
  then csv(the.file,  function(t) self:add(ROW(t,self)) end) 
       self:bins()
  else for _,row in pairs(src or {}) do self:add(row) end end end

function DATA:add(row)
  if self.cols then push(self.rows, self.cols:add(row)) else self.cols=COLS(self.cells) end end 

function DATA:bins()
  for _,row in pairs(rows) do
    for _,cols in pairs{self.cols.x, self.cols.y} do for _,col in pairs(cols) do 
      row.bins[col.at] = col:bin(x) end end end end
-------------------- ------------------- --------------------- -------------------- ----------
return {the=the, DATA=DATA, ROW=ROW, SYM=SYM, NUM=NUM, COLS=COLS}
