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
local abs  = math.abs
local big  = 1E32
local fmt  = string.format
local R    = math.random

-- bulk add  
-- (obj,{any}) --> obj 
local function adds(i,t)  
  for _,x in pairs(t or {}) do 
    i = i or (type(x)=="number" and Num() or Sym())
    i:add(x) end 
  return i end 

-- coerce   
-- (str) --> bool | num | str
local function atom(s)  
  fn = function(s) return (s=="true" and true) or (s~= "false" and s) end
  return tonumber(s) or math.tointeger(x) or fn(s:match"^%s*(.-)%s*$") end

-- Csv string to csv cells.   
-- (str,fun) --> {any}
local function cells(s,fn,    t) 
  t={}; for s1 in s:gmatch("([^,]+)") do t[1+#t]=fn(s1) end; return t end

-- Iterate over csv rows from file  
-- (str) --> {bool | num | str 
local function csv(file,     src) 
  src = io.input(file)
  return function(    line) 
    line = io.read()
    if line then return cells(line, atom) else io.close(src) end end end

-- Filter all items through `fn`  
-- ({any},fun) --> {any}
local function map(t,fn,    u)  
  u={}; for _,v in pairs(t) do u[1+#u] = fn(v) end; return u end  

local o -- defined below, needed here for klass' "__tostring"

-- make class  
-- (str) --> klass
local function klass(name) 
  local kl = {}
  kl.__index = kl
  kl.__tostring = function(self) return name .. o(self) end
  setmetatable(kl, {
    __call= function(_,...) return setmetatable({},kl):_new(...) end})
  return kl end

-- Sample from a gaussian   
-- (num,num) --> 0..1
local function normal(mu,sd) 
  return mu + sd * (-2 * math.log(R()))^.5 * math.cos(2 * math.pi * R()) end

-- Pretty print most things.    
-- (any) --> str
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
-- ({any},any) --> x
local function push(t,x) 
   t[1+#t]=x; return x end

-- Sum items, filtered by `fn`.   
-- ({any},fun) --> num
local function sum(t,fn,   n)  
  n=0; map(t,function(v) n=n+fn(v) end); return n end

----------------------------------------------------------------------
-- ## Sym

-- Keeps summaries of numeric columns
local Sym=klass"Sym"

-- Constructor
function Sym._new(i,at,txt) -- (int, str) --> Sym
  i.at  = at or 0       -- col position
  i.txt = txt or ""     -- col name
  i.n   = 0             -- items seen
  i.has = {}            -- symbol counts
  return i end

-- Update
function Sym.add(i,v) -- (any) --> nil
  if v~= "?" then i.n=i.n+1; i.has[v] = 1 + (i.has[v] or 0) end end

-- Discretize (symbols discretize to themselves).
function Sym.bin(i,x)  -- (any) --> x
  return x end

-- Symboolic diversity (how much we avoid central tendancy) is entropy.
function Sym.div(i,      P) -- () --> float
  P = function(n) return n/i.n * math.log(n/i.n, 2) end
  return sum(i.has,P) end

-- Symboolic central tendancy is model.
function Sym.mid(i,     most,mode) -- () --> any
  most=0
  for k,n in pairs(i.has) do if n > most then mode,most=k,n end end
  return mode end

-----------------------------------------------------------------------
-- ## Num
--
-- Keeps summaries of numeric columns
local Num=klass"Num"

-- Constructor
function Num._new(i,at,txt) -- (int,str) --> Num
  i.at          = at or 0
  i.txt         = txt or ""
  i.n,i.mu,i.m2 = 0,0,0
  i.lo, i.hi    = big, -big
  i.w           = tostring(txt or ""):find"-$" and 0 or 1 
  return i end

-- Update, uses Welford's algorithm (for incremntal variance calcs).
function Num.add(i,v,    d) -- (v) --> nil
  if v ~= "?" then
    i.n  = i.n + 1
    i.lo = math.min(i.lo,v)
    i.hi = math.max(i.hi,v)
    d    = v - i.mu
    i.mu = i.mu + d/i.n
    i.m2 = i.m2 + d*(v - i.mu) end end

-- Gaussian discretization.
function Num.bin(i,x)  -- (num) --> int
  return (the.bin * i:cdf(x)) // 1 end

-- CDF(x) = prob that x takes a value less than or equal to x.
function Num.cdf(i,x,    fn,z) -- (num) --> 0..1
  fn = function(z) return 1 - 0.5*math.exp(-0.717*z - 0.416*z*z) end
  z = (x - i.mid()) / i.div()
  return z>=0 and fn(z) or 1 - fn(-z) end

-- Numeric diversity (how much we avoid central tendancy) is std.dev.
function Num.div(i)  -- () --> float
   return i.n < 2 and 0 or (i.m2/(i.n-1 ))^0.5 end

-- Numeric central tendancy is mean.
function Num.mid(i)  -- () --> num
  return i.mu end

-- Normalize x to 0..1 for lo..hi
function Num.norm(i,x) -- (num) --> 0..1
  return x=="?" and x or (x - i.lo)/(i.hi - i.lo + 1/big) end

-----------------------------------------------------------------------
-- ## Cols

-- Finds, and stores, all the roles of our columns.
local Cols=klass"Cols"

-- Computes Nums and Syms from list `t` of column names.
function Cols._new(i,t,      all,x,y,klass,col)
  i.all, i.x, i.y, i.name = {},{},{},t
  for n,s in pairs(t) do
    col = push(i.all, (s:find"^[A-Z]" and Num or Sym)(n,s))
    if not s:find"X$" then
      push(s:find"[+!-]$" and i.y or i.x,col)
      if s:find"!$" then i.klass = col end end end 
  return i end

-- Update
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
  help = help..fmt('\n   %s%-6s%-8s %s',t.flag,t.arg or "",t.txt)
  egs[t.flag] = function(arg,     ok,err)
     ok,err = xpcall(function() math.randomseed(the.seed); t.fn(arg) end, 
                     debug.traceback)
     if not ok then print(">>> Error: ["..t.flag.."]", err) end
     return ok and 0 or 1 end end

for k, v in help:gmatch("%-%s*(%w+)[^%(]*%((%d+)%)") do
  assert(not the[k], "repeated field ["..k.."]")
  the[v] = atom(v) end

eg["-h"] = function(_) 
   for k in pairs(eg) do t[1+#t]=k end; table.sort(t)print(help)      end

eg["--the"] = function(_) print(o(the))    end

eg["--normal"] = function(_,t) 
  t={}
  for _ = 1,10000 do x = normal(10,1) // 1; t[x] = x + (t[x] or 0) end
  for x,n in pairs(t) do 
    print(fmt("%10s %10s : %s",x,n, ("*"):rep(n//1000))) end end

eg["--sym"] = function(_)
   print(o(adds(Sym(),{"a","a","a","a","b","b","c"}))) end}

eg["--num"] = function(_,    n) 
   n=Num()
   for _ = 1,10000 do n:add(normal(10,1)) end
   assert(abs(n:mid() - 10) < 0.02 and abs(n:div() - 1) < 0.02)  end

eg["--csv"] = function(_,    n) 
   n=Num()
   for _ = 1,10000 do n:add(normal(10,1)) end
   assert(abs(n:mid() - 10) < 0.02 and abs(n:div() - 1) < 0.02)  end}

for k,fn in pairs(eg) do eg[k:match("^(%S+)")] = fn; help=help.."\n" k end

function cli(t)
  for k, v in pairs(t) do
    v = tostring(v)
    for argv,s in pairs(arg) do
      if s=="-"..(k:sub(1,1)) or s==k then
        v = v=="true" and "false" or v=="false" and "true" or arg[argv+1]
        t[k] = atom(v) end end end

if arg[0]:find"??al.lua" then
  for n,s in pairs(arg) do
    randomseed(the.rseed)
    if eg[s] then eg[s](arg[n+1]) else
      for k,_ in pairs(the) do
        if s=="-"..k:sub(1,1) then the[k]=atom(arg[n+1]) end end end end end

   for i,s in pairs(arg) do
     if egs[s] then
       egs[s](arg[i+1]) end end end  

return {
  Sym=Sym, Num=Num, Cols=Cols, Data=Data,
  adds=adds, atom=atom, big=big, cells=cells, csv=csv, eg=eg,
  fmt=fmt, map=map, new=new, o=o, push=push, sum=sum, the=the }
