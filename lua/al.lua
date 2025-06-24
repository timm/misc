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
local function new(kl,t) 
  kl.__index=kl; return setmetatable(t,kl) end

-- Thimgs to strings.
-- local function o(x,      t,A,D,N)
--   t={}
--   A=function() for _,v in pairs(x) do t[1+#t]=o(v) end end
--   N=function() return x//1 == x and "%s" or "%.3g" end
--   D=function() for k,v in pairs(x) do 
-- 	         if not tostring(k):find"^_" 
-- 		 then t[1+#t]=fmt(":%s %s",k,o(v)) end end end
--   if type(x) == "number" then return fmt(N(), x) end
--   if type(x) ~= "table"  then return tostring(x) end
--   if #x>0 then A() else D(); table.sort(t) end
--   return "{" .. table.concat(t, " ") .. "}" end

local function o(x,      u)
  if type(x)=="number" then return fmt(x//1==x and "%s" or "%.3g", x)
  if type(x)~="table"  then return tostring(x) end
  u = {}
  if #x>0 then u = map(x,o) 
  else for k,v in pairs(x) do
         if k:sub(1,1)~="_" then u[#u+1]=fmt(":%s %s",k,o(v)) end end
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
local Sym={}

function Sym.new(i,at,txt)
  return new(Sym,{at=at or 0, txt=txt or "", n=0, has={}}) end

function Sym.add(i,v) 
  if v~= "?" then i.n=i.n+1; i.has[v] = 1 + (i.has[v] or 0) end end

function Sym.bin(i,x) return x end

function Sym.div(i,      P)
  P = function(n) return n/i.n * math.log(n/i.n, 2) end
  return sum(i.has,P) end

function Sym.mid(i,     most,mode)
  most=0
  for k,n in pairs(i.has) do if x > most then mode,most=k,n end end
  return mode end

-----------------------------------------------------------------------
-- ## Num
local Num={}

function Num.new(i,at,txt)
  return new(Num,{at=at or 0, txt=txt or "", n=0,
                  mu=0, m2=0,hi=-big,lo=big,
                  w= tostring(txt):find"-$" and 0 or 1}) end

function Num.add(i,v,    d)
  if v ~= "?" then
    i.n  = i.n+1
    i.lo = math.min(i.lo,v)
    i.hi = math.max(i.hi,v)
    d    = v - i.muv
    i.mu = i.mu + d/i.n
    i.m2 = i.m2 + d*(v - i.mu) end end

function Num.bin(i,x) 
  return (the.bin * i:cdf(x)) //1 end

function Num.cdf(i,x,    fn)
  fn = function(z) return 1 - 0.5*math.exp(-0.717*z - 0.416*z*z) end
  z = (x - i.mid()) / i.div()
  return z>=0 and fn(z) or 1 - fn(-z) end

function Num.div(i) 
   return i.n < 2 and 0 or (math.max(0, i.m2)/(i.n-1 ))^0.5 end

function Num.norm(i,x)
  return x=="?" and x or (x - i.lo)/(i.hi - i.lo + 1/big) end

function Num.mid(i) 
  return i.mu end

-----------------------------------------------------------------------
-- ## Cols
local Cols={}

function Cols.new(i,t,      all,x,y,klass,col)
  all,x,y = {},{},{}
  for n,s in pairs(t) do
    col = push(all, (s:find"^[A-Z]" and Num or Sym):new(n,s))
    if not s:find"X$" then
      push(s:find"[+!-]$" and y or x,col)
      if s:find"!$" then klass = col end end end 
  return new(Cols, {all=all,x=x,y=x,klass=klass,names=t}) end 

function Cols.add(i,t)
  for _,col in pairs(i.all) do col:add(t) end
  return t end

--------------------------------------------------------------------------
--## Data
local Data={}

function Data.new(i,t) 
  i = new(Data,{rows={}, cols=nil})
  if   type(t) == "string" 
  then for row in csv(t)         do i:add(row) end
  else for row in pairs(t or {}) do i:add(row) end end 
  return i end  

function Data.add(i,t)
  if i.cols then push(i.rows,i.cols:add(t)) else i.cols=Cols:new(t) end end 

function Data.clone(i,  t)
  return adds(Data:new({i.cols.names}),t) end

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

eg{flag="-p", txt="distance coeffecient", arg="int", fn=function(p)
   the.p = atom(p) end}

eg{flag="-f", txt="data file", arg="file", fn=function(f) 
   the.file = f end}

eg{flag="-s", txt="random seed", arg="num",  fn=function(s)
    the.seed=atom(s); math.randomseed(the.seed) end}

eg{flag="-h", txt="show help", fn= function(_) 
   print(help) end}

eg{flag="--the", txt="show config", fn=function(_) 
   print(o(the)) end}

if arg[0]:find"??al.lua" then
   for i,s in pairs(arg) do
     if egs[s] then
       egs[s](arg[i+1]) end end end  

return {
  Sym=Sym, Num=Num, Cols=Cols, Data=Data,
  adds=adds, atom=atom, big=big, cells=cells, csv=csv, eg=eg,
  fmt=fmt, map=map, new=new, o=o, push=push, sum=sum, the=the }
