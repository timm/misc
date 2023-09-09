#!/usr/bin/env lua
local lint,list,maths,obj,rand,run,settings,str = require"lib"
local the=settings.create[[

bins: discretize

USAGE: ./key.lua [OPTIONS]

OPTIONS
  -b --bins      number of bins         = 5
  -d --decimals  print first `decimals` = 2
  -f --file      csv file to load       = 
  -g --go        start up action        = nothing
  -h --help      show help              = false]]

-------------------- ------------------- --------------------- -------------------- ----------
local csv,kap,oo,push,sort = str.csv,list.kap,str.oo,list.push,list.sort
local cos,exp,log,max,min,pi = math.cos,math.exp,math.log,math.max,math.min,math.pi
local DATA,ROW,NUM,SYM=obj"DATA", obj"ROW", obj"NUM", obj"SYM"

-------------------- ------------------- --------------------- -------------------- ----------
function SYM:init(at,txt) 
  return {n=0,at=at or 0,txt=txt or "",most=0,mode=nil,has={}} end

function SYM:add(x)
  if x~="?" then 
    self.n = self.n + 1 
    self.has[x] = 1 + (self.has[x] or 0)
    if self.has[x] > self.most then 
      self.most, self.mode = self.has[x],x end end end

function SYM:bin(x) return x end

-------------------- ------------------- --------------------- -------------------- ----------
function NUM:init(at,txt) 
	return {is=NUM, n=0,at=at or 0,txt=txt or "",mu=0,m2=0,sd=0} end  

function NUM:add(x,    d)
  if x ~="?" then 
    self.n  = self.n + 1 
    d       = x - self.mu
    self.mu = self.mu + d/self.n 
    self.m2 = self.m2 + d*(x - self.mu) 
    self.sd = exp(-.5*((x - self.mu)/self.sd)^2) / (self.sd*((2*pi)^0.5)) 
    self.lo = min(self.lo,x)
    self.hi = max(self.hi,x) end end

NUM._bins= {
    [ 3] = { -.43,	 .43},
    [ 4] = { -.67,     0,	 .67},
    [ 5] = { -.84,	 -.25,	 .25,  .84},
    [ 6] = { -.97,	 -.43,    0,	 .43,  .97},
    [ 7] = { -1.07,	-.57,	-.18,	 .18,  .57, 1.07},
    [ 8] = { -1.15,	-.67,	-.32, 	 0,	 .32,  .67, 1.15},
    [ 9] = { -1.22,	-.76,	-.43,	-.14,	 .14,	 .43,  .76,	1.22},
    [10] = { -1.28,	-.84,	-.52,	-.25,	   0,	 .25,  .52,	 .84,	1.28}}

function NUM:bin(x,     tmp)
  if x=="?"      then return x end
  if col.is==SYM then return x end 
  tmp = (x- col.mu)/col.sd
  for b,x in pairs(breaks[the.bins])  do if tmp <= x then return b end end
  return the.bins end
  
-------------------- ------------------- --------------------- -------------------- ----------
function COLS:init(t)
  self.all={}; self.x={}; self.y={}; self.names=t
  for at,txt in pairs(t) do 
    col = push(self.all, (txt:find"^A-Z" and NUM or SYM)(at,txt))
    if txt:find"X$" then
      push(txt:find"[+-]$" and self.y or self.x, col) end end end
   
-------------------- ------------------- --------------------- -------------------- ----------
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
function DATA:init(src)
  self.rows={}
  if   type(s) == "string" 
  then csv(the.file,  function(t) self:add(data,ROW(t,data)) end) 
       self:bins()
  else for _,row in pairs(src or {}) do self:add(data,row) end
  end       
  return data end

function DATA:all(row,fun,     x)
  for _,cols in pairs{self.cols.x, self.cols.y} do 
    for _,col in pairs(cols) do 
      x= row[col.at]
      if x ~= "?" then fun(col,x) end end end end
   
function DATA:add(row)
    if   self.cols 
    then push(self.rows, row)
         self:all(row, function(col,x) col:add(x) end) 
    else self.cols = COLS(self.cells) end end 

function DATA:bins()
  for _,row in pairs(rows) do
    self:all(row, function(col,x) row.bins[col.at] = col:bin(x) end) end end

-------------------- ------------------- --------------------- -------------------- ----------

return {the=the, DATA=DATA, ROW=ROW, SYM=SYM, NUM=NUM, COLS=COLS}
