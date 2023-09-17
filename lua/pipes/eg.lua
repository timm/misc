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
  or lua egs.lua [OPTIONS] [-g ACTION]

OPTION:
  -b --bins      number of bins             = 5
  -d --decimals  print first `decimals`     = 2
  -f --file      csv file to load           = ../data/auto93.csv
  -F --Far       distance to far            = .9
  -g --go        start up action            = nothing
  -h --help      show help                  = false
  -H --Half      items explored in halving  = 256
  -s --seed      random number seed         = 937162211
]]
-------------------- ------------------- --------------------- -------------------- ----------
local l = require("lib")
local obj,oo,push,rnd,sort   = l.obj, l.str.oo, l.list.push, l.maths.rnd, l.list.sort
local cos,exp,log,max,min,pi = math.cos,math.exp,math.log,math.max,math.min,math.pi

local DATA,COLS,ROW,NUM,SYM = obj"DATA", obj"COLS", obj"ROW", obj"NUM", obj"SYM"
-------------------- ------------------- --------------------- -------------------- ----------
--  _    ._ _  
-- _> \/ | | | 
--    /        
function SYM:init(t, at, txt) 
  self.at, self.txt = at or 0, txt or ""
  self.n, self.has, self.most, self.mode = 0, {}, 0, nil
  for _,x in pairs(t or {}) do self:add(x) end end

function SYM:add(x,n)
  if x~="?" then 
    n = n or 1
    self.n = self.n + n 
    self.has[x] = n + (self.has[x] or 0)
    if self.has[x] > self.most then 
      self.most, self.mode = self.has[x],x end end end

function SYM:bin(x)    return x end
function SYM:dist(x,y) return x==y and 0 or 1 end
function SYM:mid()     return self.mode end
function SYM:div()     return l.list.entropy(self.has) end
-- -------------------- ------------------- --------------------- -------------------- ----------
-- ._      ._ _  
-- | | |_| | | | 

function NUM:init(t, at, txt) 
  self.at, self.txt = at or 0, txt or ""
  self.n, self.mu, self.m2, self.sd = 0, 0, 0, 0
  for _,x in pairs(t or {}) do self:add(x) end end

function NUM:add(x,    d)
  if x ~="?" then 
    self.n  = self.n + 1 
    d       = x - self.mu
    self.lo = min(self.lo,x)
    self.hi = max(self.hi,x) 
    self.mu = self.mu + d/self.n 
    self.m2 = self.m2 + d*(x - self.mu) 
    if i.n > 1 then self.sd = sqrt(self.m2/(i.n - 1)) end end end

function NUM:mid()   return self.mu end
function NUM:div()   return self.sd end
function NUM:d2h(x)  return abs(self:norm(x) - self.heaven) end
function NUM:norm(x) return x=="?" and x or (x-self.lo)/(self.hi - self.lo + 1E-30) end

function NUM:dist(x,y) 
  if x=="?" and y=="?" then return 1 end
  x,y = self:norm(x),self:norm(y)
  if x=="?" then x = y<.5 and 1 or 0 end
  if y=="?" then y = x<.5 and 1 or 0 end
  return abs(x - y)  end

function NUM:bin(x,     tmp)
  if x=="?"      then return x end
  tmp = (x - col.mu)/col.sd
  for b,x in pairs(NUM._bins[the.bins])  do if tmp <= x then return b end end
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
    col = (txt:find"^[A-Z]" and NUM or SYM)({}, at, txt)
    push(self.all, col)
    if not txt:find"X$" then
      category = txt:find"[+-]$" and self.y or self.x
      push(category, col) end end end

function COLS:add(row)
  for _,cols in pairs{self.cols.x, self.cols.y} do for _,col in pairs(cols) do 
    col:add(row.cells[col.at]) end end
  return row end
-------------------- ------------------- --------------------- -------------------- ----------
-- ._  _       
-- |  (_) \/\/ 

function ROW:init(t,data) return {_data=data,rows=t; bins=l.list.copy(t),cost=0} end

function ROW:dist(i,j)
  d,n = 0,0
  for _,col in pairs(row1._data.cols.x) do
    n = n + 1
    d = d + (col:dist(row1.dist[col.at], row2.dist[col.at]))^the.p end
  return (d/n)^(1/the.p) end

function ROW:neighbors(rows)
  return l.list.keysort(rows, function(row2) return self:dist(row2) end) end

function ROW:extremities(rows,     n,x,y)
  n = (#rows*the.Far)//1
  x = self:neighbors(rows)[n]
  y = x:neighbors(rows)[n]
  return x,y, x:dist(y) end

function ROW:d2h()
  d,n,self.cost = 0,0,1
  for _,col in pairs(row1._data.cols.y) do 
    n = n + 1
    d = d + col:d2h(self.cells[col.at])^the.p end
  return (d/n)^(1/the.p) end

function ROW.better(row1,row2) return row1:d2h() < row2:d2h() end
-------------------- ------------------- --------------------- -------------------- ----------
--  _|  _. _|_  _. 
-- (_| (_|  |_ (_| 

function DATA:init(src)
  self.rows={}
  if   type(src) == "string" 
  then l.str.csv(the.file,  function(t) self:add(ROW(t,self)) end) 
       self:bins()
  else for _,row in pairs(src or {}) do self:add(row) end end end

function DATA:clone(rows)
  data = DATA({self.cols.names})
  for row in pairs(rows or {}) do data:add(row) end 
  return data end

function DATA:add(row)
  if self.cols then push(self.rows, self.cols:add(row)) else self.cols=COLS(self.cells) end end 

function DATA:bins()
  for _,row in pairs(rows) do
    for _,cols in pairs{self.cols.x, self.cols.y} do for _,col in pairs(cols) do 
      row.bins[col.at] = col:bin(x) end end end end

function DATA:stats(  cols,swant,n,      t)
  t = {N = #self.rows}
  for _,c in pairs(cols or self.cols.y) do t[c.at]=rnd(swant=="div" and c:div() or c:mid(), n) end
  return t end
-------------------- ------------------- --------------------- -------------------- ----------
local tree={}

function tree.half(rows,sorted,     a,b,C,as,bs,some, cosine)
  some  = l.rand.many(rows, min(the.Half, #rows))
  a,b,C = some[1]:extremities(some)
  as,bs = {},{}
  if sorting and b:better(a) then a,b = b,c end
  function cosine(A,B) return (A^2+C^2 - B^2)/(2*C) end
  for n,row in pairs(sorted(rows, function(r) return cosine(r:dist(a), r:dist(b)) end)) do
    push(n <= #rows/2 and as or bs, row) end
  return a,b,as,bs end

function tree.grow(data,sorted)
  function _grow(data1)
    node = {here=data1}
    if #data1.rows > 2* ((#data.rows)^.5) then
      _,__,lefts,rights = tree.half(data1.rows,sorted)
      node.lefts        = _grow(data:clone(lefts))
      node.rights       = _grow(data:clone(rights)) end 
    return node end 
  return _grow(data) end

function tree.walk(node,fun,lvl)
  if node then
    lvl = lvl and lvl + 1 or 0
    fun(node, lvl, not (node.lefts or node.rights))
    tree.walk(node.lefts, fun,lvl)
    tree.walk(node.rights,fun,lvl) end end 

function tree.show(node,     _show)
  function _show(node1,lvl,leafp,     n,post)
    n    = (#node.here.rows)^.5
    n    = log(n,2)//1
    pre  = "%".. tostring(n) .. "s %s"
    post = leafp and o(node.here:stats()) or ""
    print(string.format(pre, ("|...")*rep(lvl),post)) end
  tree.walk(node, _show) end
-------------------- ------------------- --------------------- -------------------- ----------
return {the=the, DATA=DATA, ROW=ROW, SYM=SYM, NUM=NUM, COLS=COLS}
