local b4={}; for k, _ in pairs(_ENV) do b4[k]=k end

local the = {
  what = "new.lua",
  why  = "simple inference",
  when = "(c) 2025, MIT License",
  who  = "Tim Menzies",
  rseed = 1234567891, 
  csv   = "../data/auto93m.csv",
  m     = 2, 
  k     = 1 
}

local BIG=1E32
local eg,Data,Num,Sym,Cols = {},{},{},{},{}
------------------------------------------------------------------------------
local fmt,num,lt,sort,push,new,map,sum,olist,odict,o,word,words,csv

fmt=string.format

function num(x) return x=="?" and BIG or x end

function lt(x) return function(t,u) return num(t[x]) < num(u[x]) end end

function sort(t,fun) table.sort(t,fun); return t end

function push(t,x) t[1+#t] = x; return x end

function new(isa,i) isa.__index=isa; setmetatable(i,isa); return i end

function map(t, fun,     u)
  u={}; for _,x in pairs(t) do push(u, fun(x)) end; return u end

function sum(t, fun,      n)
  n=0; for _,x in pairs(t) do n = n + fun(x) end; return n end

function olist(x,   t)
  t={}; for _,v in pairs(x) do t[1+#t]=o(v) end; return t end

function odict(x,   t)
  t={}; for k,v in pairs(x) do t[1+#t]=fmt(":%s %s",k,o(v)) end
  return sort(t) end

function o(x)
  return type(x) == "number" and fmt(x//1 == x and "%s" or "%.3g",x) or (
         type(x) ~= "table"  and tostring(x)                         or (
         "{".. table.concat(#x>0 and olist(x) or odict(x)," ") .."}" )) end 

function word(s) return tonumber(s) or s:match("^%s*(.-)%s*$") end

function words(s,   t) 
  t={}; for s1 in s:gmatch"([^,]+)" do t[1+#t]=word(s1) end; return t end

function csv(src,fun,     s)
  src = io.input(src)
  s = io.read()
  while s do fun( words(s) ); s=io.read() end
  io.close(src) end

function adds(t,  col)
  for _,x in pairs(t) do
    if   col 
    then col:add(x) 
    else return type(x)=="number" and adds(t,Num()) or adds(t,Sym()) end end
  return col end

------------------------------------------------------------------------------
function Num:new(txt,at)
   return new(Num,{txt=txt or " ", at=at or 0, n=0, 
                   mu=0, sd=0, m2=0, hi= -BIG, lo= BIG,
                   goal = tostring(txt):find"-$" and 0 or 1}) end  

function Num:add(x,     d)
  if x ~= "?" then
    self.n   = self.n + 1
    d        = x - self.mu
    self.mid = self.mid + d/self.n
    self.m2  = self.m2 + d*(x - self.mu)
    if x > self.hi then self.hi = x end
    if x < self.lo then self.lo = x end end 
  return x end

function Num:sub(x):
  if x ~= "?" then
    self.n = self.n - 1
    if   self.n < 2 
    then self.mu,self.sd = 0,0 
    else d       = x - self.mu
         self.mu = self.mu - d / self.n
         self.m2 = self.m2 - d * (x - self.mu) end end 
  return x end

function Num:mid() return self.mu end

function Num:spread() 
  return self.n<2 and 0 or (math.max(0,self.m2)/(self.n-1))^0.5 end

function Num:norm(x)
  return x=="?" and x or (x - self.lo) / (self.hi - self.lo) end

------------------------------------------------------------------------------
function Sym:new(txt, at)
   return new(Sym, {txt=txt or "", at=at or 0, n=0, 
                    has={}, most=0, mode=nil}) end

function Sym:add(x)
  if x ~= "?" then
    self.n   = self.n + 1
    self.has[x] =  1 + (self.has[x] or 0)
    if self.has[x] > self.most then
      self.most, self.mode = self.has[x], x end end 
  return x end
 
function Sym:sub(x)
  if x ~= "?" then
    self.n   = self.n + 1
    i.has[x] -= 1 end 
  return x end

function Sym:mid() return self.mode end

function Sym:spread(    _p) 
  _p = function(v) return v/self.n * math.log(v/self.n,2) end
  return - sum(self.has, _p) end

------------------------------------------------------------------------------
function Cols:new(names)
   local x,y,all,klass = {}, {}, {}, nil
   for at,txt in pairs(names) do
      local col = push(all, (txt:find"^[A-Z]" and Num or Sym):new(txt,at))
      if not txt:find"X$" then 
         push(txt:find"[!+-]$" and y or x, col)
         if txt:find"!$" then klass=col end end end
   return new(Cols,{x=x,y=y,all=all,klass=klass, names=names}) end 

function Cols:add(row)
   for _,col in pairs(self.all) do col:add(row[col.at]) end
   return row end

------------------------------------------------------------------------------
function Data:new(src)
  self = new(Data, {rows={}, cols=nil})
  (type(src)=="string" and csv or map)(src, function(r) self:add(r) end)
  return self end

function Data:add(row)
  if   self.cols
  then push(self.rows, self.cols:add(row)) 
  else self.cols = Cols:new(row) end 
  return row end

function Data:tree(  eps,min)
  _ys = function(row) return self:ydist(row) end
  for _,col in pairs(self.cols.all) do
    rows = sort(self.rows, lt(col.at))
    rhs  = adds(map(self.rows, _ys))
    lhs  = Num()
    for i,row in pairs(rows) do
       

------------------------------------------------------------------------------
eg["--the"]= function(_) print(o(the)) end
eg["--csv"]= function(_) for _,r in pairs(csv(the.csv)) do print(o(r)) end end

------------------------------------------------------------------------------
if not pcall(debug.getlocal,4,1) then  
  for n,s in pairs(arg) do
    math.randomseed(the.rseed)
    if eg[s] then eg[s](arg[n+1]) else
      for k,_ in pairs(the) do 
        if s=="-"..k:sub(1,1) then the[k]=word(arg[n+1]) end end end end end

for k,v in pairs(_ENV) do if not b4[k] then print("?",k,type(k)) end end 
