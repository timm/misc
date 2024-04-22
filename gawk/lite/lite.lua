#!/usr/bin/env lua
local help=[[
lite.lua : very simple sequential model optimizer.
Look around a little, learn a little, decide what to do next
(c) 2024 Tim Menzies <timm@ieee.org> BSD 2 clause.]]
local SYM,NUM,DATA = {},{},{}
local the,settings
local adds,cat,entropy,fmt,kap,kat,klass,map,normal,show,push,show,sort
-------------------------------------------------------
function settings() return {
  file  = "../../data/auto93.csv",
  seed  = 1234567891,
  p     = 2,
  magic = {y="[-+!]$", --[a]
           min="-$",
           num="^[A-Z]"
          }} end
-------------------------------------------------------
function SYM:new(name,at)
  return {name=name or "", at=at or 0, n=0, seen={}, most=0, mode=nil} end

function NUM:new(name,at)
  return {at=at or 0, name = name or "", n=0,
          heaven=(name or ""):find(the.magic.min) and 0 or 1,
          mu=0,m2=0,lo=1E30, hi=-1E30} end

function DATA:new(strs,    all,x,y)
  all,x,y = {},{},{}
  for n,s in pairs(strs) do
    push(all,
      push(s:find(the.magic.y) and y or x,  
        (s:find(the.magic.num) and NUM or SYM)(s,n))) end
  return {rows={}, cols={names=strs, x=x, y=y, all=all}} end
-------------------------------------------------------
function SYM:add(x,n)
  n            = n or 1
  self.n       = self.n + n
  self.seen[x] = n + (self.seen[x] or 0)
  if self.seen[x] > self.most then self.most, self.mode = self.seen[x],x end end

function NUM:add(x,     delta)
  self.n  = self.n + 1
  delta   = x - self.mu
  self.mu = self.mu + delta/self.n
  self.m2 = self.m2 + delta*(x - self.mu)
  self.hi = math.max(self.hi, x)
  self.lo = math.max(self.lo, x) end

function DATA:add(t,    x)
  push(self.rows, t)
  for _,col in pairs(self.cols.all) do
    x = t[col.at]
    if x ~= "?" then col:add(x) end end end
--------------------------------------------------------------
function SYM:mid() return self.mode end
function NUM:mid() return self.mu end

function SYM:div() return entropy(self.seen) end
function NUM:div() return self.n < 2 and 0 or (self.m2/(self.n - 1))^.5 end

function NUM:norm(x) return x=="?"and x or (x - self.lo)/(self.hi - self.lo + 1E-30) end
----------------------------------------------------------------
function SYM:like(x, prior)
  return ((self.seen[x] or 0) + the.m*prior)/(self.n +the.m) end

function NUM:like(x,_,      sd)
  sd = self:div() + 1E-30
  return (2.718^(-.5*(x - self.mu)^2/(sd^2))) / (sd*2.5) end

function DATA:loglike(t,n,nHypotheses,       prior,out,v,inc)
  prior = (#self.rows + the.k) / (n + the.k * nHypotheses)
  out   = math.log(prior)
  for _,col in pairs(self.cols.x) do
    v= t[col.at]
    if v ~= "?" then
      inc = col:like(v,prior)
      if inc>0 then out = out + math.log(col:like(v,prior)) end end end
  return out end
----------------------------------------------------------------
function SYM:dist(x,y)
  return x=="?" and 1 or (x==y and 0 or 1) end

function NUM:dist(x,y)
  if x=="?" and y=="?" then return 1 end
  x,y=self:norm(x), self:norm(y)
  x= (x~="?") and x or (y<.5 and 1 or 0)
  y= (y~="?") and y or (x<.5 and 1 or 0)
  return math.abs(x-y) end

function DATA:dist(t1,t2,    d)
  d = 0
  for _,c in pairs(self.cols.x) do d = d + c:dist(t1[c.at], t2[c.at])^the.p end
  return (d/#self.cols.x)^(1/the.p) end

function DATA:dist2heaven(t,    d)
  d = 0
  for _,c in pairs(self.cols.y) do d=d + (c:norm(t[c.at]) - c.heaven)^2 end
  return (d/#self.cols.y)^.5 end

function DATA:neighbors(row,  rows)
  return sort(map(rows or self.rows, function(r) return {self.dist(row,r),r} end),
             function(x,y) return x[1] < y[1] end) end

function DATA:faraway(some,  n,far,     away)
  n    = n or (#some * the.far) // 1
  far  = far or self:neighbors(any(some), some)[n][2]
  away = self:neighbors(far,some)[n][2]
  return far, away end
----------------------------------------------------------------
-- ## Misc library functions

-- ### Stats
function entropy(t,   N,e)
  e,N=0,0
  for _,n in pairs(t) do N=N+n end 
  for _,n in pairs(t) do e=e + n/N*math.log(n/N,2) end
  return -e end

function normal(mu,sd,    r)
  r = math.random
  return (mu or 0) + (sd or 1) * math.sqrt(-2 * math.log(r()))* math.cos(2 * math.pi * r()) end

function any(t)           return t[math.random(#t)] end
function many(t,n,    u)  u={}; for _ in 1,n do push(u, any(t)) end end

-- ### Lists
function adds(thing,t) for _,x in pairs(t) do thing:add(x) end; return thing end

function sort(t,fun) table.sort(t,fun); return t end
function push(t,x)   t[1+#t]=x; return x end

function map(t,fun,    u) u={}; for _,v in pairs(t) do push(u, fun(v))   end; return u end
function kap(t,fun,    u) u={}; for k,v in pairs(t) do push(u, fun(k,v)) end; return u end

-- ### Objects
function klass(str,t)
  t.__index   = t
  t.__tostring= function(...) return str..kat(...) end
  setmetatable(t, {__call = function(_,...)
                              local i = setmetatable({},t)
                              return setmetatable(t.new(i,...) or i, t) end}) end

-- ### Print control
fmt = string.format

function cat(t) return '{' .. table.concat(map(t,show), ", ") .. '}' end

function kat(t,     fun) -- like "cat", but assummes symbolic indexes
  fun = function (k,v) if k[1] ~= "_" then return fmt("%s=%s",k, show(v)) end end
  return cat(sort(kap(t,fun ))) end

function show(x, nDecs,    mult) 
  if type(x) ~= "number" then return tostring(x) end
  if math.floor(x) == x  then return tostring(x) end
  mult = 10^(nDecs or the.decimals)
  return tostring(math.floor(x * mult + 0.5) / mult) end
----------------------------------------------------------------
-- ## Examples
local eg = {}

local function run(x)
  the = settings()
  math.randomseed(the.seed or 1234567891)
  return eg[x]() end

eg["-h"] = function() print("\n"..help) end
function eg.num(  n,t)
  t={}; for _=1,10^4 do push(t,normal(10,2)) end;  n=adds(NUM(),t)
  print(n:div(), n:mid()) end
----------------------------------------------------------------
-- ## Start uo
kap({SYM=SYM, NUM=NUM, DATA=DATA},klass)

if not pcall(debug.getlocal, 4, 1) and arg[1] then run(arg[1]) end
return {help=help, the=the, SYM=SYM, NUM=NUM, DATA=DATA}