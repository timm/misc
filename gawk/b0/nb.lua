#!/usr/bin/env lua
-- vim: set tabstop=2 shiftwidth=2 expandtab :
local help=[[
kah.lua : a tiny AI workbench
(c) 2024, Tim Menzies <timm@ieee.org>, MIT license

USAGE:
  ./nb.lua [OPTIONS] [x.csv]

OPTIONS:
  --the         show options
  --help        show help
  --csv   x.csv demo: printing rows from csv
  --cols        demo: show cols initialized from row names
  --like  x.csv demo: show row liklihood calc]]

local abs,exp,log,min,max = math.abs, math.exp, math.log, math.min, math.max
local sqrt, pi = math.sqrt,  math.pi
local sum,coerce,csv,o,new,push,shuffle,sort,map,keysort
local eg, the = {}, {
  k=1,
  m=2,
  p=2,
  seed=1234567891, 
  big=1E32}

-------------------------------------------------------------------------------
local Num,Sym,Cols,Data = {},{},{},{}

function Num:new(txt,at) 
  return new(Num, {at=at or 0, txt=txt or "", n=0, 
                   mu=0, m2=0, sd=0, lo = math.huge, hi= -math.huge,
                   goal = (txt or ""):find"-$" and 0 or 1}) end

function Sym:new(txt,at) 
  return new(Sym, {at=at or 0, txt=txt or "", n=0, has={},most=0,mode=nil}) end

function Cols:new()
  return new(Cols, {names={},all={}, y={}, x={}, klass=nil,initialized=false}) end

function Data:new() 
  return new(Data, {rows={}, cols=Cols:new()}) end

function Data:clone(rows)
   return Data:new():add(self.cols.names):adds(rows or {}) end

-------------------------------------------------------------------------------
function Sym:add(x)
  self.n = self.n + 1
  self.has[x] = 1 + (self.has[x] or 0)
  if self.has[x] > self.most then self.most, self.mode=self.has[x], x end end 

function Num:add(x,    d)
  self.n  = self.n + 1
  self.lo = min(x, self.lo)
  self.hi = max(x, self.hi)
  d = x - self.mu
  self.mu = self.mu + d/self.n
  self.m2 = self.m2 + d*(x - self.mu)
  self.sd = self.n < 2 and 0 or (self.m2 / (self.n - 1))^0.5 end 

function Data:adds(src)
  if   type(src)=="table" 
  then for _,row in pairs(src) do self:add(row) end 
  else for   row in csv(src)   do self:add(row) end end
  return self end
    
function Data:add(row)
  if   self.cols.initialized
  then push(self.rows, self.cols:add(row))
  else self.cols:initialize(row) end 
  return self end

function Cols:add(row)
  for at,x in pairs(row) do
    if x ~="?" then
      self.all[at]:add(x) end end
  return row end 

function Cols:initialize(names,    col)
  self.names = names
  for at,s in pairs(self.names) do
    col = push(self.all, (s:find"^[A-Z]" and Num or Sym):new(s,at))
    if s:find"!$" then self.klass = col end
    push(s:find"[!+-]$" and self.y or self.x, col) end 
  self.initialized = true 
  return self end

-------------------------------------------------------------------------------
function Num:norm(x)
  return x=="?" and x or (x-self.lo) / (self.hi - self.lo + 1/the.big) end

function Num.pooledSd(i,j)
  return sqrt(((i.n-1)*i.sd^2 + (j.n-1)*j.sd^2)/(i.n+j.n-2)) end

function Data:klass(row) return row[self.cols.klass.at] end

-------------------------------------------------------------------------------
-- `like(x:atom, ?prior: float) -> float`  
-- Returns nil if `x` is a missing value.  
function Sym:like(x,  prior,    v,tmp) --> num
  if x~="?" then
    return ((self.has[x] or 0) + the.m*prior) / (self.n + the.m) end end

-- Ditto. Ignores second argument (the `prior`).
-- For an explanation of this function, see 
-- https://chatgpt.com/share/6744b232-a680-8010-a20b-99a0d5fdd364
function Num:like(x,_,     v,tmp)
  if x~="?" then
    v = self.sd^2 + 1/the.big
    tmp = exp(-1*(x - self.mu)^2/(2*v)) / (2*pi*v) ^ 0.5
    return max(0,min(1, tmp + 1/the.big)) end end

function Data:loglike(row,  nall,nh,    prior,LIKE,LOG)
  LIKE = function(col) return LOG( col:like(row[col.at], prior) ) end
  LOG  = function(x)   return x and x>0 and log(x) or 0 end
  prior= (#self.rows + the.k) / (nall + the.k*nh)
  return LOG(prior) + sum(self.cols.x, LIKE) end

function Data.classify(row, datas,    most,out,nh,nall,tmp)
  most,nh,nall = -math.huge,0,0
  for _,data in pairs(datas) do nh = nh + 1; nall = nall + #data.rows end
  for k,data in pairs(datas) do
     tmp = data:loglike(row,nall,nh)
     if tmp > most then most,out = tmp,k end end
  return out,most end

function Sym:dist(a,b) 
  return  a=="?" and b=="?" and 1 or  (a==b and 0 or 1) end

function Num:dist(a,b)
  a,b = self:norm(a), self:norm(b)
  a = a ~= "?" and a or (b<0.5 and 1 or 0)
  b = b ~= "?" and b or (a<0.5 and 1 or 0)
  return abs(a-b) end

function Data:xdist(row1,row2,    DIST) 
  DIST= function(col) return col:dist(row1[x.at], row2[x.at])^the.p  end
  return (sum(self.cols.x, DIST) / #self.cols.x) ^ (1/the.p) end

function Data:neighbors(row1,  rows) 
  return keysort(rows or self.rows, function(row2) return self:xdist(row1,row2) end) end

function Data:ydist(row,     Y)
  Y= function(col) return abs(col:norm(row[col.at]) - col.goal)^2 end
  return sqrt(sum(self.cols.y, Y)) end

-------------------------------------------------------------------------------
local l={}

function l.push(t,x) t[1+#t]=x; return x end

function l.shuffle(t,    k) 
  for j = #t,2,-1 do k=math.random(j); t[j],t[k] = t[k],t[j] end; return t end

function l.sort(t,FUN) table.sort(t,FUN); return t end

function l.map(t,FUN,   u) 
  u={}; for _,v in pairs(t) do u[1+#u]=FUN(v) end; return u end

function l.sum(t,f,   n)
  n=0; for _,x in pairs(t) do n=n+f(x) end; return n end

function l.keysort(t,FUN,       DECORATE, UDECORATE) 
  DECORATE   = function(x) return {FUN(x),x} end
  UNDECORATE = function(x) return x[2] end
  return map(sort(map(t,DECORATE),lt(1)), UNDECORATE) end

function l.coerce(s,     FUN,TRIM) 
  TRIM= function(s) return s:match"^%s*(.-)%s*$" end
  FUN = function(s) return s=="true" and true or s ~= "false" and s end
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
  FMT = string.format
  NUM = function() return x//1 == x and tostring(x) or FMT("%.3g",x) end
  LIST= function() for k,v in pairs(x) do t[1+#t] = l.o(v) end end
  DICT= function() for k,v in pairs(x) do t[1+#t] = FMT(":%s %s",k, l.o(v)) end end
  if type(x) == "number" then return NUM() end 
  if type(x) ~= "table"  then return tostring(x) end
  if #x>0 then LIST() else DICT(); table.sort(t) end
  return "{" .. table.concat(t, " ") .. "}" end

function l.new(kl,t)
  kl.__index=kl; kl.__tostring = l.o; return setmetatable(t,kl) end

-------------------------------------------------------------------------------
function eg.help(_) print("\n"..help.."\n") end

function eg.the(_) print(o(the)) end
function eg.csv(f) for row in csv(f) do print(o(row)) end end

function eg.data(f)
  for _,col in pairs(Data:new():adds(f).cols.all) do 
    print(o(col)) end end

function eg.cols(_,    names)
  names={"cylinders", "Displacement", "HorsepowerX", 
         "Weight-", "Acceleration+", "Model", "origin", "Mpg+"}
  for _,col in pairs(Cols:new():initialize(names).all) do print(o(col)) end end

function eg.like(f,    d)
  d=Data:new():adds(f)
  for k,row in pairs(shuffle(d.rows)) do 
    if k>20 then break else print(row[#row],d:like(row,1000,2)) end end end

-------------------------------------------------------------------------------
sum,coerce,csv,o,new,push = l.sum,l.coerce,l.csv,l.o,l.new,l.push
shuffle,sort,map,keysort = l.shuffle, l.sort, l.map, l.keysort

math.randomseed(the.seed)

if arg[0]:find"nb.lua" then
  for j,s in pairs(arg) do
     s=s:gsub("^[-][-]","")
     if eg[s] then  eg[s](arg[j+1]) end 
     math.randomseed(the.seed) end end

return {Num=Num, Sym=Sym, Data=Data, the=the, lib=l} 
