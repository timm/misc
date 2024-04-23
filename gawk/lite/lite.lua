#!/usr/bin/env lua
local l,eg,help={},{},[[
lite.lua : very simple sequential model optimizer.
Look around a little, learn a little, decide what to do next
(c) 2024 Tim Menzies <timm@ieee.org> BSD 2 clause.]]

local ROW,SYM,NUM,DATA = {},{},{},{}
local the,settings
-----------------------------------------------------------------------------------------
function settings() return {
  file  = "../../data/auto93.csv",
  seed  = 1234567891,
  decs  = 3,
  nb    = {k=    1,
           m=    2},
  dist  = {p=    2,
           far=  .92},
  magic = {y=    "[-+!]$", --[a]
           min=  "-$",
           num=  "^[A-Z]"
          }} end
-----------------------------------------------------------------------------------------
function ROW:new(t) return {cells=t} end

function SYM:new(name,at)
  return {name=name or "", at=at or 0, n=0, seen={}, most=0, mode=nil} end

function NUM:new(name,at)
  return {at=at or 0, name = name or "", n=0,
          heaven=(name or ""):find(the.magic.min) and 0 or 1,
          mu=0,m2=0,lo=1E30, hi=-1E30} end

function DATA:new(strs,    all,x,y)
  all,x,y = {},{},{}
  for n,s in pairs(strs) do
    l.push(all,
      l.push(s:find(the.magic.y) and y or x,  
        (s:find(the.magic.num) and NUM or SYM)(s,n))) end
  return {rows={}, cols={names=strs, x=x, y=y, all=all}} end
-----------------------------------------------------------------------------------------
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
  self.lo = math.min(self.lo, x) end

function DATA:add(row)
  l.push(self.rows, row)
  for _,col in pairs(self.cols.all) do
    x = row.cells[col.at]
    if x ~= "?" then col:add(x) end end end
-----------------------------------------------------------------------------------------
function SYM:mid() return self.mode end
function NUM:mid() return self.mu end

function SYM:div() return l.entropy(self.seen) end
function NUM:div() return self.n < 2 and 0 or (self.m2/(self.n - 1))^.5 end

function NUM:norm(x) return x=="?"and x or (x - self.lo)/(self.hi - self.lo + 1E-30) end
-----------------------------------------------------------------------------------------
function SYM:like(x, prior)
  return ((self.seen[x] or 0) + the.nb.m*prior)/(self.n +the.nb.m) end

function NUM:like(x,_,      sd)
  sd = self:div() + 1E-30
  return (2.718^(-.5*(x - self.mu)^2/(sd^2))) / (sd*2.5) end

function DATA:loglike(t,n,nHypotheses,       prior,out,v,inc)
  prior = (#self.rows + the.nb.k) / (n + the.nb.k * nHypotheses)
  out   = math.log(prior)
  for _,col in pairs(self.cols.x) do
    v = t.cells[col.at]
    if v ~= "?" then
      inc = col:like(v,prior)
      if inc > 0 then out = out + math.log(inc) end end end
  return out end
-----------------------------------------------------------------------------------------
function DATA:smo(  score,likike)
  score = score or function(B,R) return  B - R end
  like  = function(row,data) return data:loglike(row, len(data.rows),2) end
  acquire = function (best,rest,rows) end 
  end
-----------------------------------------------------------------------------------------
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
  for _,c in pairs(self.cols.x) do 
    d = d + c:dist(t1.cells[c.at], t2.cells[c.at])^the.dist.p end
  return (d/#self.cols.x)^(1/the.dist.p) end

function DATA:dist2heaven(t,    d)
  d = 0
  for _,c in pairs(self.cols.y) do d=d + (c:norm(t[c.at]) - c.heaven)^2 end
  return (d/#self.cols.y)^.5 end

function DATA:neighbors(row,  rows)
  return l.sort(l.map(rows or self.rows, function(r) return {self.dist(row,r),r} end),
                      function(x,y) return x[1] < y[1] end) end

function DATA:faraway(rows,  n,far,     away)
  n    = n or (#rows * the.dist.far) // 1
  far  = far or self:neighbors(l.any(rows), rows)[n][2]
  away = self:neighbors(far,rows)[n][2]
  return far, away end
-----------------------------------------------------------------------------------------
-- ## Misc library functions

-- ### Stats
function l.entropy(t,   N,e)
  e,N=0,0
  for _,n in pairs(t) do N=N+n end 
  for _,n in pairs(t) do e=e + n/N*math.log(n/N,2) end
  return -e end

function l.normal(mu,sd,    r)
  r = math.random
  return (mu or 0) + (sd or 1) * math.sqrt(-2*math.log(r()))* math.cos(2*math.pi*r()) end

function l.any(t)           return t[math.random(#t)] end
function l.many(t,n,    u)  u={}; for _ in 1,n do l.push(u, l.any(t)) end end

-- ### Lists
function l.adds(thing,t) for x in pairs(t) do thing:add(x) end; return thing end

function l.sort(t,fun) table.sort(t,fun); return t end
function l.push(t,x)   t[1+#t]=x; return x end

function l.map(t,fun,    u)
  u={}; for _,v in pairs(t) do l.push(u, fun(v))  end;  return u end

function l.kap(t,fun,    u)
  u={}; for k,v in pairs(t) do l.push(u, fun(k,v)) end; return u end

-- ### Objects
function l.klass(str,t)
  t.__index   = t
  t.__tostring= function(...) return str..l.kat(...) end
  setmetatable(t, {__call = function(_,...)
                              local i = setmetatable({},t)
                              return setmetatable(t.new(i,...) or i, t) end}) end

-- ### Write control
l.fmt = string.format

function l.cat(t) return '{' .. table.concat(l.map(t,l.show), ", ") .. '}' end

function l.kat(t,     fun) -- like "cat", but assummes symbolic indexes
  fun = function (k,v) if k[1] ~= "_" then return l.fmt("%s=%s",k, l.show(v)) end end
  return l.cat(l.sort(l.kap(t,fun ))) end

function l.show(x, nDecs,    mult) 
  if type(x) ~= "number" then return tostring(x) end
  if math.floor(x) == x  then return tostring(x) end
  mult = 10^(nDecs or the.decs)
  return tostring(math.floor(x * mult + 0.5) / mult) end

-- ### Read control
function l.coerce(s1,    fun) 
  function fun(s2)
    return s2=="true" or (s2~="false" and s2) or false end 
  return math.tointeger(s1) or tonumber(s1) or fun(s1:match'^%s*(.*%S)') end

function l.cells(s,   t)
  t={}; for s1 in s:gmatch("([^,]+)") do t[1+#t]=l.coerce(s1) end; return t end

function l.csv(src,     i)
  i,src = 0,src=="-" and io.stdin or io.input(src)
  return function(      s)
    s = io.read()
    if s then i = i+1; return i,l.cells(s) else io.close(src) end end end

function l.arg(s)
  for k,v in pairs(arg) do if v==s then return l.coerce(arg[k+1]) end end end

function l.csv2data(file,     data)
  for i,t in l.csv(file) do
    if i==1 then data=DATA(t) else data:add(ROW(t)) end end
  return data end
-----------------------------------------------------------------------------------------
-- ## Examples

local function run(x)
  the = settings()
  math.randomseed(the.seed or 1234567891)
  return eg[x]() end

eg["-h"]= function() print("\n"..help) end

eg["--num"]= function(  n,t)
  t={}; for _=1,10^4 do l.push(t, l.normal(10,2)) end;  n=l.adds(NUM(),t)
  print(n:div(), n:mid()) end

eg["--csv"]= function(    i)
  i=0
  for row in l.csv(the.file) do i=i+1; if i%50==1 then print(l.cat(row)) end end end

eg["--data"]=function(    d)
  d =  l.csv2data(the.file)
  for c,col in pairs(d.cols.all) do print(c,col) end
  print(#d.rows, d.cols.all[1]:div()) end
-----------------------------------------------------------------------------------------
-- ## Start uo
l.kap({ROW=ROW,SYM=SYM, NUM=NUM, DATA=DATA},l.klass)
if not pcall(debug.getlocal, 4, 1) and arg[1] then run(arg[1]) end
return {help=help, the=the, lib=l, ROW=ROW, SYM=SYM, NUM=NUM, DATA=DATA}