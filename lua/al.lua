#!/usr/bin/env lua
local the={_about = {
	      what = "ap.lua",
              why  = "simple active learner",
              when = "(c) 2025",
	      who  = "Tim Menzies",
              how  = "MIT License"},
           p=2, 
           bins=10, 
	   seed=1234567891,
           file="../../moot/classify/weather.csv"}

-----------------------------------------------------------------
--## Misc utils
local big  = 1E32
local fmt  = string.format
local R    = math.random

-- bulk add
local function adds(i,t) 
  for _,x in pairs(t or {}) do 
    i = i or (type(x)=="number" and Num() or Sym())
    i:add(x) end 
  return i end 

-- coerce
local function atom(s) 
  return tonumber(s) or s:match"^%s*(.-)%s*$" end

-- Csv string to csv cells.
local function cells(s,fn,    t)
  t={}; for s1 in s:gmatch("([^,]+)") do t[1+#t]=fn(s1) end; return t end

-- Iterate over csv rows from file
local function csv(file,     src)
  src = io.input(file)
  return function(    line) 
    line = io.read()
    if line then return cells(line, atom) else io.close(src) end end end

-- Filter all items through `fn`
local function map(t,fn,    u) 
  u={}; for _,v in pairs(t) do u[1+#u] = fn(v) end; return u end  

-- make class
local o -- defined below, needed here for "__tostring"

local function klass(name)
  local kl = {}
  kl.__index = kl
  kl.__tostring = function(self) return name .. o(self) end
  setmetatable(kl, {
    __call= function(_,...) return setmetatable({},kl):_new(...) end})
  return kl end

local function normal(mu, sd)
  return mu + sd * (-2 * math.log(R()))^.5 * math.cos(2 * math.pi * R()) end

function o(x,      u,_yes)
  if type(x)=="number" then return fmt(x//1==x and "%s" or "%.3g", x) end
  if type(x)~="table"  then return tostring(x) end
  u = {}
  _yes = function(k) return  tostring(k):sub(1,1)~="_" end 
  if #x>0 then u = map(x,o) else
    for k,v in pairs(x) do if _yes(k) then u[#u+1]=fmt(":%s %s",k,o(v)) end end
    table.sort(u) end
  return "{" .. table.concat(u, " ") .. "}" end 

-- Push `x` to end of  list, return `x`.
local function push(t,x)
   t[1+#t]=x; return x end

-- Sum items, filtered by `fn`.
local function sum(t,fn,   n) 
  n=0; map(t,function(v) n=n+fn(v) end); return n end

----------------------------------------------------------------------
-- ## Sym
local Sym=klass"Sym"

function Sym._new(i,at,txt)
  i.at  = at or 0
  i.txt = txt or ""
  i.n   = 0
  i.has = {}
  return i end

function Sym.add(i,v) 
  if v~= "?" then i.n=i.n+1; i.has[v] = 1 + (i.has[v] or 0) end end

function Sym.bin(i,x) return x end

function Sym.div(i,      P)
  P = function(n) return n/i.n * math.log(n/i.n, 2) end
  return sum(i.has,P) end

function Sym.mid(i,     most,mode)
  most=0
  for k,n in pairs(i.has) do if n > most then mode,most=k,n end end
  return mode end

-----------------------------------------------------------------------
-- ## Num
local Num=klass"Num"

function Num._new(i,at,txt)
  i.at          = at or 0
  i.txt         = txt or ""
  i.n,i.mu,i.m2 = 0,0,0
  i.lo, i.hi    = big, -big
  i.w           = tostring(txt or ""):find"-$" and 0 or 1 
  return i end

function Num.add(i,v,    d)
  if v ~= "?" then
    i.n  = i.n + 1
    i.lo = math.min(i.lo,v)
    i.hi = math.max(i.hi,v)
    d    = v - i.mu
    i.mu = i.mu + d/i.n
    i.m2 = i.m2 + d*(v - i.mu) end end

function Num.bin(i,x) 
  return (the.bin * i:cdf(x)) // 1 end

function Num.cdf(i,x,    fn,z)
  fn = function(z) return 1 - 0.5*math.exp(-0.717*z - 0.416*z*z) end
  z = (x - i.mid()) / i.div()
  return z>=0 and fn(z) or 1 - fn(-z) end

function Num.div(i) 
   return i.n < 2 and 0 or (i.m2/(i.n-1 ))^0.5 end

function Num.mid(i) 
  return i.mu end

function Num.norm(i,x)
  return x=="?" and x or (x - i.lo)/(i.hi - i.lo + 1/big) end

-----------------------------------------------------------------------
-- ## Cols
local Cols=klass"Cols"

function Cols._new(i,t,      all,x,y,klass,col)
  i.all, i.x, i.y, i.name = {},{},{},t
  for n,s in pairs(t) do
    col = push(i.all, (s:find"^[A-Z]" and Num or Sym)(n,s))
    if not s:find"X$" then
      push(s:find"[+!-]$" and i.y or i.x,col)
      if s:find"!$" then i.klass = col end end end 
  return i end

function Cols.add(i,t)
  for _,col in pairs(i.all) do col:add(t) end
  return t end

--------------------------------------------------------------------------
--## Data
local Data=klass"Data"

function Data._new(i,src) 
  i.rows, i.cols = {}, nil
  return type(src)=="string" and i:fromFile(src) or adds(i,src) end 

function Data.add(i,t)
  if i.cols then push(i.rows, i.cols:add(t)) else i.cols=Cols(t) end end 

function Data.clone(i,  t)
  return adds(Data({i.cols.names}),t) end

function Data.fromFile(i,file)
  for u in csv(file) do i:add(u) end; return i end

-----------------------------------------------------------------------
-- ## Command-line

local egs={}
local help=fmt("\n%s %s\n%s, %s, %s\n",the._about.what, the._about.why, 
                               the._about.when, the._about.who, the._about.how)

local function eg(t)
  help = help..fmt('\n   %-6s%-8s %s',t.flag,t.arg or "",t.txt)
  egs[t.flag] = function(arg,     ok,err)
     ok,err = xpcall(function() math.randomseed(the.seed); t.fn(arg) end, 
                     debug.traceback)
     if not ok then print(">>> Error: ["..t.flag.."]", err) end
     return ok and 0 or 1 end end

eg{flag="-b", txt="number of bins", arg="int", fn=function(b)
   the.bins = atom(b) end}

eg{flag="-p", txt="distance term", arg="int", fn=function(p)
   the.p = atom(p) end}

eg{flag="-f", txt="data file", arg="file", fn=function(f) 
   the.file = f end}

eg{flag="-s", txt="random seed", arg="num",  fn=function(s)
    the.seed=atom(s); math.randomseed(the.seed) end}

eg{flag="-h", txt="show help", fn= function(_) 
   print(help) end}

eg{flag="--the", txt="show config", fn=function(_) 
   print(o(the)) end}

eg{flag="--normal", txt="test normal", fn=function(_,t,x) 
  t={}
  for _ =1,10000 do x=(normal(10,1) // .5)*.5; t[x] = x + (t[x] or 0) end
  for x,n in pairs(t) do 
    print(fmt("%10s %10s : %s",x,n, ("*"):rep(n//500))) end end}

eg{flag="--sym", txt="test sym", fn=function(_) 
   print(o(adds(Sym(),{"a","a","a","a","b","b","c"}))) end}

eg{flag="--num", txt="test sym", fn=function(_,    n) 
   n=Num()
   for _ = 1,10000 do n:add(normal(10,1)) end
   print(n:mid(), n:div()) end}

if arg[0]:find"??al.lua" then
   for i,s in pairs(arg) do
     if egs[s] then
       egs[s](arg[i+1]) end end end  

return {
  Sym=Sym, Num=Num, Cols=Cols, Data=Data,
  adds=adds, atom=atom, big=big, cells=cells, csv=csv, eg=eg,
  fmt=fmt, map=map, new=new, o=o, push=push, sum=sum, the=the }
