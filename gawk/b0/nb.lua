#!/usr/bin/env lua
-- vim: set tabstop=2 shiftwidth=2 expandtab :

local b4={}; for k,_ in pairs(_ENV) do b4[k]=k end
local function rogues()
  for k,v in pairs(_ENV) do if not b4[k] then print("?", k, type(v)) end end end

local the,eg = {k=1,m=2,seed=1234567891, big=1E32},{}
local sum,coerce,csv,o,new,push,shuffle
local exp,log,min,max,pi = math.exp, math.log, math.min, math.max, math.pi

-------------------------------------------------------------------------------
local Num,Sym,Cols,Data = {},{},{},{}

function Num:new(txt,at) 
  return new(Num, {at=at or 0, txt=txt or "", n=0, 
                   mu=0, m2=0, sd=0, lo=the.big, hi= -the.big,
                   goal = (txt or ""):find"-$" and 0 or 1}) end

function Sym:new(txt,at) 
  return new(Sym, {at=at or 0, txt=txt or "", n=0, has={},most=0,mode=nil}) end

function Cols:new()
  return new(Cols, {all={}, y={}, x={}, klass=nil,initialized=false}) end

function Data:new() 
  return new(Data, {rows={}, cols=Cols:new()}) end

function eg.data(f)
  for _,col in pairs(Data:new():adds(f).cols.all) do 
    print(o(col)) end end

function eg.cols(_,    names)
  names={"cylinders", "Displacement", "HorsepowerX", 
         "Weight-", "Acceleration+", "Model", "origin", "Mpg+"}
  for _,col in pairs(Cols:new():initialize(names).all) do print(o(col)) end end

-------------------------------------------------------------------------------
function Sym:add(x)
  self.n = self.n + 1
  self.has[x] = 1 + (self.has[x] or 0)
  if self.has[x] > self.most then self.most, self.mode=self.has[x], x end end 

function Num:add(x,    d)
  self.lo = min(x, self.lo)
  self.hi = max(x, self.hi)
  self.n  = self.n + 1
  d = x - self.mu
  self.mu = self.mu + d/self.n
  self.m2 = self.m2 + d*(x - self.mu)
  self.sd = self.n < 2 and 0 or (self.m2 / (self.n - 1))^0.5 end 

function Data:adds(src,      FILE,LIST)
  if     src == nil or tyoe(src) == "string" 
  then   for   row in csv(src) do self:add(row) end 
  else   for _,row in pairs(t) do self:add(row) end end
  return self end
    
function Data:add(row)
  if   self.cols.initialized
  then push(self.rows, self.cols:add(row))
  else self.cols:initialize(row) end end

function Data:klass(row) return row[self.cols.klass.at] end

function Cols:add(row)
  for at,x in pairs(row) do
    if x ~="?" then
      self.all[at]:add(x) end end
  return row end 

function Cols:initialize(names,    col)
  for at,s in pairs(names) do
    col = push(self.all, (s:find"^[A-Z]" and Num or Sym):new(s,at))
    if s:find"!$" then self.klass = col end
    push(s:find"[!+-]$" and self.y or self.x, col) end 
  self.initialized = true 
  return self end

-------------------------------------------------------------------------------
function Sym:like(x,prior,    v,tmp) 
  if x~="?" then
    return ((self.has[x] or 0) + the.m*prior) / (self.n + the.m) end end

function Num:like(x,_,     v,tmp)
  if x~="?" then
    v = self.sd^2 + 1/the.big
    tmp = exp(-1*(x - self.mu)^2/(2*v)) / (2*pi*v) ^ 0.5
    return max(0,min(1, tmp + 1/the.big)) end end

function Data:like(row,  nall,nh,    prior,LIKE,LOG) 
  LIKE = function(col) return LOG( col:like(row[col.at], prior) ) end
  LOG  = function(x)   return x and x>0 and log(x) or 0 end
  prior= (#self.rows + the.k) / (nall + the.k*nh)
  return LOG(prior) + sum(self.cols.x, LIKE) end

function eg.like(f,    d)
  d=Data:new():adds(f)
  for k,row in pairs(shuffle(d.rows)) do 
    if k>20 then break else print(row[#row],d:like(row,1000,2)) end end end

-------------------------------------------------------------------------------
local l={}

function l.push(t,x) t[1+#t]=x; return x end

function l.shuffle(t,    k) 
  for j = #t,2,-1 do k=math.random(j); t[j],t[k] = t[k],t[j] end; return t end

function l.sum(t,f,   n)
  n=0; for _,x in pairs(t) do n=n+f(x) end; return n end

function l.coerce(s,     FUN,TRIM) 
  TRIM = function(s) return s:match"^%s*(.-)%s*$" end
  FUN  = function(s) return s=="true" and true or s ~= "false" and s end
  return math.tointeger(s) or tonumber(s) or FUN(TRIM(s)) end

function l.csv(file,     src) 
  if file ~="-" then src=io.input(file) end
  return function(     s,t)
    s = io.read()
    if s 
    then t={}; for s1 in s:gmatch"([^,]+)" do t[1+#t]=l.coerce(s1) end; return t 
    else if src then io.close(src) end end end end

function l.o(x,        t,FMT,NUM,LIST,DICT) 
  t = {}
  FMT  = string.format
  NUM  = function() return x//1 == x and tostring(x) or FMT("%.3g",x) end
  LIST = function() for k,v in pairs(x) do t[1+#t] = l.o(v) end end
  DICT = function() for k,v in pairs(x) do t[1+#t] = FMT(":%s %s",k, l.o(v)) end end
  if type(x) == "number" then return NUM() end 
  if type(x) ~= "table"  then return tostring(x) end
  if #x>0 then LIST() else DICT(); table.sort(t) end
  return "{" .. table.concat(t, " ") .. "}" end

function l.new(kl,t)
  kl.__index=kl; kl.__tostring = l.o; return setmetatable(t,kl) end

function eg.the(_) print(o(the)) end
function eg.csv(f) for row in csv(f) do print(o(row)) end end

-------------------------------------------------------------------------------
sum,coerce,csv,o,new,push = l.sum,l.coerce,l.csv,l.o,l.new,l.push
shuffle = l.shuffle

math.randomseed(the.seed)

if arg[0]:find"nb.lua" 
then for j,s in pairs(arg) do
       s=s:gsub("^[-][-]","")
       if eg[s] then  eg[s](arg[j+1]) end 
       math.randomseed(the.seed) end
     rogues() 
else return {Num=Num, Sym=Sym, Data=Data, the=the, lib=l} end
