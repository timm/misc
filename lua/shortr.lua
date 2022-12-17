local help = [[
acquire.lua : row/column clustering for rep grids
(c)2022, Tim Menzies <timm@ieee.org>, BSD-2 

Reads a csv file where row1 is column names and col1,coln
is dimension left,right and all other cells are numerics.

USAGE:   lua acquire.lua  [OPTIONS]

OPTIONS:
  -d  --dump  on crash, dump stack       = false
  -f  --file  csv file                   = /User/timm/gits/timm/tested/etc/data/auto93.csv
  -F  --Far   where to find far things   = 1
  -g  --go    start-up action            = nothing
  -h  --help  show help                  = false  
  -m  --min   min size for leaf clusters = .5
  -p  --p     distance coefficient       = 2
  -s  --seed  random number seed         = 937162211
  -x  --X     small x change             = .1
]]
local b4={}; for k,_ in pairs(_ENV) do b4[k] = k end
local the,coerce,column,csv,kap,map,o,oo,push,sort,settings = {}
local SYM,NUM,COLS,DATA = {},{},{},{}
----------------------------------------------------------------------------------------------------
local is,oid,obj,new,isa,_={},0
function obj(s)      local x={_txt=s}; _=x; return x end
function new(kl,...)  oid=oid+1; local x={_is=kl, _id=oid}; kl.new(x,...); return x end 

function o(t)
  if type(t)~="table" then return tostring(t) end
  local pre,fun,isPublic =  t._is and  t._is._txt or ""
  function isPublic(k) return tostring(k):sub(1,1)~="_" end
  function fun(k,v) if isPublic(k)  then return string.format(":%s %s",k,o(v)) end end
  return pre.."{"..table.concat(#t>0 and map(t,o) or sort(kap(t,fun))," ").."}" end
----------------------------------------------------------------------------------------------------
local add
function add(i,x,...)  return i._is.add(i,x,...) end
----------------------------------------------------------------------------------------------------
function column(i,n,s)
  i.at, i.txt = n or 0, s or ""
  i.n=0
  i.isGoal = i.txt:find"[+-!]$"
  i.isKlass = i.txt:find"[!]$"
  i.isIgnored = i.txt:find"X$"
  i.w = i.txt:find"-$" and -1  or 1 end

NUM = obj"NUM"
function _.new(i,n,s) 
  column(i,n,s)
  i.lo =  math.huge
  i.hi = -math.huge
  i.mu,i.m2=0,0  end

function _.add(i,n,...) 
   if n ~= "?" then
     i.n = i.n+1
     local d = n - i.mu
     i.mu = i.mu + d/i.n
     i.m2 = i.m2 + d*(n - i.mu)
     i.lo = math.min(n, i.lo)
     i.hi = math.max(n, i.hi) end end

function _.mid(i) return i.mu end
function _.div(i)
   return (i.m2 <0 or i.n < 2) and 0 or (i.m2/(i.n-1))^0.5  end
----------------------------------------------------------------------------------------------------
SYM=obj"SYM"
function _.new(i,n,s)
  column(i,n,s)
  i.mode, i.most= nil,0
  i.has = {} end

function _.add(i,x,inc)
   if n ~= "?" then
     inc = inc or 1
     i.n = i.n+inc
     i.has[x] = inc + (i.has[x] or 0)
     if i.has[x] > i.most then i.most, i.mode = i.has[x], x end end end

function _.mid(i) return i.mode end
function _.div(i)
   local function P(p) return p*math.log(p,2) end
   local e=0; for x,n in pairs(i.has) do e = e + P(n/i.n) end; return -e end
----------------------------------------------------------------------------------------------------
COLS=obj"COL"
function _.new(i,t)
  i.all,i.x,i.y,i.klass = {},{},{}
  for n,s in pairs(t) do
    local col= new(s:find"^[A-Z]+" and NUM or SYM, n,s)
    push(i.all, col)
    if not col.isIgnored then
      if col.isKlass then i.klass = col end
      push(col.isGoal and y or x, col) end end end

function _.add(i,t)
  for _,cols in pairs{i.x,i.y} do
    for _,col in pairs(cols) do add(col, t[col.at]) end end 
  return t end 
----------------------------------------------------------------------------------------------------
DATA=obj"DATA"
function _.new(i,src)
  i.rows,i.cols={},nil
  local add=function(x) i._add(x) end 
  if type(src)=="string" then csv(src, add) else map(src or {}, add) end end

function _.add(i,t)
  t=t.cells and t or ROW(t) 
  if i.cols then push(i.rows, add(i.cols,t)) else i.cols=new(COLS,t) end end
----------------------------------------------------------------------------------------------------
-- ## Strings to things
function coerce(s) --> any; return int or float or bool or string from `s`
  local function word(s)
    if s=="true" then return true elseif s=="false" then return false else return s end end
  return math.tointeger(s) or tonumber(s) or word(s:match"^%s*(.-)%s*$") end

function csv(sFilename,fun) --> nil; call `fun` on rows (after coercing cell text)
  local src,s,t  = io.input(sFilename)
  s = io.read()
  while s do 
    t={}; for s1 in s:gmatch("([^,]+)") do push(t,coerce(s1)) end; fun(t)
    s = io.read() end
  io.close(src) end 

function settings(t,s)
  s:gsub("\n[%s]+[-]([%S])[%s]+[-][-]([%S]+)[^\n]+= ([%S]+)", function(k,key,v) 
    for n,x in ipairs(arg) do
      if x=="-"..k or x=="--"..key then
        v = v=="false" and "true" or v=="true" and "false" or arg[n+1] end end
    t[key] = coerce(v) end)
  if t.help then os.exit(print(s)) end
  return t end

-- ### Lists
function push(t,x) t[1+#t]=x; return x end

function sort(t,fun) table.sort(t,fun) return t end --> t; returns `t` sorted by `fun` 

function map(t,fun) --> t; returns copy of `t`, all items filtered by `fun`.
  local u={}; for __,x in pairs(t) do push(u,fun(x)) end; return u end

function kap(t,fun) --> t; returns copy of `t`, all items filtered by `fun`.
  local u={}; for k,x in pairs(t) do push(u,fun(k,x))  end; return u end

-- ### Printing
function oo(t) print(o(t)); return t end 
-----------------------------------------------------------------------------------------------------
local eg={}
function eg.all()
  for k,fun in pairs(eg) do
    if k ~="all" then
      settings(the,help)
      math.randomseed(the.seed)
      fun() end end end

function eg.nothing() return end
function eg.the() oo(the) end
function eg.num() oo(add(new(NUM),20)) end
-----------------------------------------------------------------------------------------------------
the = settings(the,help)
eg[the.go]()
for k,v in pairs(_ENV) do if not b4[k] then print("?",k,type(v)) end end
