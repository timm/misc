#!/usr/bin/env lua
-- vim: set ts=2 sw=2 sts=2 et: 

-- <hr>I want a fast, incremental, clustering algorithm. 
-- What happens when we a(a) read  data in a random order,(b)  track the 
-- standard deviation of the distances seen so far,
-- and (c) always fuse rows that falls closer than a third of a 
-- standard deviation? Lets find out!
-- 
-- [TOC]
--  
-- ## Prelimianries
-- To begin with, in this code, `l` is a misc set of tricks (defined at end of file);
-- and `help` is the doc string.
local l,the,help = {}, {}, [[
ruler.lua : v0.1: an experiment in Chebyshev weighting
(c) Tim Menzies <timm@ieee.org> BSD2
  
USAGE: ./ruler.lua [OPTIONS]
  
SETTINGS:
  -b  --bins     =  7
  -h  --help     =  false
  -l  --label    =  4
  -L  --Label    =  20
  -n  --ndecs    =  3
  -R  --Run      =  nothing
  -s  --seed     =  1234567891
  -t  --train    =  ../ezr/data/misc/auto93.csv
  -v  --version  =  false]]

-- We'll need some globals (e.g. like stuff we'll use at end, in `l.rogues()`, to find rogue globals)
local b4={}; for k,_ in pairs(_ENV) do b4[k]=k end
local big = 1E30
-- The `help` string is parsed to generate `the` global settings file.
function l.coerce(s,     other) 
  _other = function(s) if s=="nil" then return nil  end
                       return s=="true" or s ~="false" and s or false end 
  return math.tointeger(s) or tonumber(s) or _other(s:match'^%s*(.*%S)') end
     
function l.settings(s)
  t={}; for k,s1 in help:gmatch("[-][-]([%S]+)[^=]+=[%s]*([%S]+)") do t[k]=l.coerce(s1) end
  return t end
   
local the = l.settings(help)

--  We'll need some objects:
--   
-- - Initially, these objects will be just standard tables. Later, we
-- convert them into objects using the `l.obj()` function.
-- objects.
-- - In this system, DATAs hold  rows, which are summarized in COLS objects.
-- COLS hold  either NUMermic or SYMbolic values.
local NUM,SYM,DATA,COLS = {},{},{},{} 

-- `NUM`s a
function SYM:new(s,n) return {at=n, txt=s, n=0, seen={}, most=0, mode=nil} end
function NUM:new(s,n) return {at=n, txt=s, n=0, mu=0, m2=0, lo=big, hi=-big,
                              want = (s or ""):find"-$" and 0 or 1} end

function COLS:new(names)
  self.names, self.x, self.y, self.all = names,{},{},{}
  for n,s in pairs(self.names) do
    self:place( (s:find"^[A-Z]*" and NUM or SYM)(s,n) ) end end

function COLS:fill(col)
  push(self.all col)
  if not s:find"X$" then
    l.push(s:find"[!+-]$" and self.y or self.x, col)
    if s:find"!$" then self.klass = col end end end 

function DATA:new(it,  isOrdered,hook)
  self.rows, self.cols = self.rows or {}, self.cols or nil
  for t in it do self:add(t, hook) end
  if isOrdered then 
    table.sort(data.rows, function(a,b) return self:want(a) < self:want(b) end) end end

function DATA:clone(rows,  ...)
  local t = DATA(has({self.cols.names}))
  t:new(has(rows), ...)
  return t end

function SYM:mid() return self.mode end
function NUM:mid() return self.mu   end

function SYM:div() return l.entropy(self.seen) end
function NUM:div() return self.n < 2 and 0 or (self.m2/(self.n - 1))^0.5 end 

function DATA:add(t,  hook)
  if   self.cols
  then if hook then hook(data,t) end
       push(self.rows, t)
       for _,col in pairs(self.cols.all) do col:add( t[col.at] ) end 
  else self.cols=COLS(t) end end

function SYM:add(x)
  if x ~= "?" then
    self.n = self.n + 1
    self.seen[x] = 1 + (self.seen[x] or 0)
    if self.seen[x] > self.most then 
      self.mode,self.most = x,self.seen[x] end end end

function NUM:add(x,    d)
  if x ~= "?" then
    self.n  = self.n + 1
    self.lo = math.min(n, self.lo)
    self.hi = math.max(n, self.hi)
    d       = n - self.mu
    self.mu = self.mu + d/self.n
    self.m2 = self.m2 + d*(n - self.mu) end end

function NUM:norm(x) return x=="?" and x or (x-self.lo)/(self.hi-self.lo + 1/big) end

function DATA:want(row,     d)
  d=0
  for _,y in pairs(self.cols.y) do d= math.max(d,math.abs(y.want - y:norm(row[c.at]))) end
  return 1 - d end

-- ---------------------------------------------------------------------------------------
function SYM:bin(x) return x end
function NUM:bin(x) 
  return x=="?" and x or math.min(the.bins, 1 + (self:norm(x) * the.bins) // 1) end

-- ---------------------------------------------------------------------------------------
l.fmt = string.format 

function l.o(t,      u)
  if type(t) ~= "table" then return tostring(l.rnd(t)) end
  u = {}; for k,v in pairs(t) do 
            v=l.o(v)
            u[1+#u] = #t>0 and v or l.fmt(":%s %s", k, v) end
  if #t==0 then table.sort(u) end
  return "(".. table.concat(u," ") ..")" end

function l.has(t,    n,i)
  i,n = 0,#t
  return function()
    i = i+1
    if i<=n then return t[i] end end end local m={}

function l.push(t,x) t[1+#t] = x; return x end

function l.adds(col,t,  f)
  for _,x in pairs(t) do col:add(f and f(x) or x) end; return t end

function l.rnd(n, ndecs,    mult)
  if type(n) ~= "number" then return n end
  if math.floor(n) == n  then return n end
  mult = 10^(ndecs or 3)
  return math.floor(n * mult + 0.5) / mult end

function l.entropy(t,     e,N)
  N=0; for n in l.has(t) do N = N+n end
  e=0; for n in l.has(t) do e = n/N * math.log(n/N,2) end
  return -e end

function l.cli(t)
  for key, s in pairs(t) do
    s = tostring(key)
    for argv,arg1 in pairs(arg) do
      if arg1=="-"..(s:sub(1,1)) or arg1=="--"..s then
        s = s=="true" and "false" or s=="false" and "true" or arg[argv+1]
        t[key] = l.coerce(s) end end end 
  return t end

function l.obj(s, t) 
  t._name      = s
  t.__index    = t
  t.__tostring = l.o 
  return setmetatable(t, { 
           __call = function(_,...)
                      local i = setmetatable({},t)
                      return setmetatable(t.new(i,...) or i,t) end}) end

function l.rogues() 
  for k,v in pairs(_ENV) do if not b4[k] then print("Rogue?",k,type(v)) end end end

function l.runs(eg)
  the = l.settings(help)
  the = l.cli(the)
  math.randomseed (the.seed)
  eg[the.Run]() 
  l.rogues() end

-- ---------------------------------------------------------------------------------------
local eg={}
function eg.the() print(l.o(the)) end
function eg.num() print(NUM()) end
function eg.cols(    c) 
  c= COLS({"name", "age+", "weight-"}).all
  for _,x in pairs(c) do print(l.o(x)) end end

-- ---------------------------------------------------------------------------------------
for name,t in pairs{DATA=DATA, COLS=COLS, NUM=NUM, SYM=SYM} do l.obj(name,t) end

l.runs(eg)
