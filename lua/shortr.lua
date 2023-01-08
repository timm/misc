local the,help = {}, [[
acquire.lua : row/column clustering for rep grids
(c)2022, Tim Menzies <timm@ieee.org>, BSD-2 

Reads a csv file where row1 is column names and col1,coln
is dimension left,right and all other cells are numerics.

USAGE:   lua acquire.lua  [OPTIONS]

OPTIONS:
  -d  --dump  on crash, dump stack       = false
  -f  --file  csv file                   = ~/gits/timm/tested/etc/data/auto93.csv
  -F  --Far   where to find far things   = 1
  -g  --go    start-up action            = nothing
  -h  --help  show help                  = false  
  -H  --half  size of sample in half     = 256
  -m  --min   min size for leaf clusters = .5
  -p  --p     distance coefficient       = 2
  -s  --seed  random number seed         = 937162211
  -x  --X     small x change             = .1
]]
local b4={}; for k,_ in pairs(_ENV) do b4[k] = k end
local any,many,coerce,column,csv,kap,lt,map,o,oo,push,sort,settings 
local SYM,NUM,ROW,COLS,DATA = {},{},{},{}
----------------------------------------------------------------------------------------------------
local is,id,obj,new,isa,_={},0
function obj(s,    t,new) --> t; create a klass and a constructor + print method
  local function new(k,...) id=id+1; local i=setmetatable({_id=id},k); t.new(i,...); return i end
  t={_is=s, __tostring = o}
  t.__index = t;return setmetatable(t,{__call=new}) end
----------------------------------------------------------------------------------------------------
function column(i,n,s)
  i.at, i.txt = n or 0, s or ""
  i.n=0
  i.isGoal = i.txt:find"[+!-]$"
  i.isKlass = i.txt:find"[!]$"
  i.isIgnored = i.txt:find"X$"
  i.w = i.txt:find"-$" and -1  or 1 end

NUM = obj"NUM"
function NUM.new(i,n,s) 
  column(i,n,s)
  i.lo =  math.huge
  i.hi = -math.huge
  i.mu,i.m2=0,0  end

function NUM.add(i,n,...) 
  if n ~= "?" then
    i.n = i.n+1
    local d = n - i.mu
    i.mu = i.mu + d/i.n
    i.m2 = i.m2 + d*(n - i.mu)
    i.lo = math.min(n, i.lo)
    i.hi = math.max(n, i.hi) end 
  return i end

function NUM.mid(i) return i.mu end
function NUM.div(i)
   return (i.m2 <0 or i.n < 2) and 0 or (i.m2/(i.n-1))^0.5  end

function NUM.norm(i,x)
  return x=="?" and x or (x-i.lo)/(i.hi - i.lo + 1E-32) end

function NUM.dist(i,row1,row2)
  local x,y = row1.cells[i.at], row2.cells[i.at]
  x,y = self:norm(x), self:norm(y)
  return x=="?" and y=="?" and 1 or math.abs(x-y)^the.p end
----------------------------------------------------------------------------------------------------
SYM=obj"SYM"
function SYM.new(i,n,s)
  column(i,n,s)
  i.mode, i.most,i.has= nil,0,{} end

function SYM.add(i,x,inc)
   if n ~= "?" then
     inc = inc or 1
     i.n = i.n+inc
     i.has[x] = inc + (i.has[x] or 0)
     if i.has[x] > i.most then i.most, i.mode = i.has[x], x end end 
   return i end

function SYM.mid(i) return i.mode end
function SYM.div(i)
   local function P(p) return p*math.log(p,2) end
   local e=0; for x,n in pairs(i.has) do e = e + P(n/i.n) end; return -e end

function SYM.dist(i,row1,row2)
  local x,y = row1.cell[i.at], row2.cell[i.at]
  return  x=="?" and y=="?" and 1 or x==y and 0 or 1 end
----------------------------------------------------------------------------------------------------
COLS=obj"COL"
function COLS.new(i,t)
  i.all,i.x,i.y,i.klass = {},{},{}
  for n,s in pairs(t) do
    local col= push(i.all, s:find"^[A-Z]+" and NUM(n,s) or SYM(n,s))
    if not col.isIgnored then
      if col.isKlass then i.klass = col end
      push(col.isGoal and i.y or i.x, col) end end end

function COLS.add(i,t)
  for _,cols in pairs{i.x,i.y} do
    for _,col in pairs(cols) do add(col, t.cells[col.at]) end end 
  return t end 
----------------------------------------------------------------------------------------------------
ROW=obj"ROW"
function ROW.new(i,t) i.cells=t end
----------------------------------------------------------------------------------------------------
DATA=obj"DATA"
function DATA.new(i,src)
  i.rows,i.cols={},nil
  local add=function(x) i:add(x) end 
  if type(src)=="string" then csv(src, add) else map(src or {}, add) end end

function DATA.add(i,t)
  t=t.cells and t or ROW(t) 
  if i.cols then push(i.rows, add(i.cols,t)) else i.cols=new(COLS,t) end end

function DATA.dist(i,row1,row2)
  local d=0; for _,col in pairs(t.cols.x) do 
               n = n + 1
               d = d + col:dist(row1,row2)^the.p end
  return (d/n)^(1/the.p) end

function DATA.far(i,row,rows)
  rows = rows or self.rows
  return sort(map(rows, function(r) return {dist=i:dist(row1,row2), row=row2} end),
              lt"dist")[the.far * (#rows)//1].row end

function DATA.half(i,    rows,b4)
  local some = many(rows, the.half)
  local left = b4 or any(some)
  local right= i:far(left, some)
  local c = self:dist(left,right)
  function proj(row,    a,b) 
    a,b= i:dist(row,left) i:dist(row,right)
    return {row=row, dist=(a^2 + c^2 - b^2)/(2*c)} end 
  local lefts,rights,mid,midc={},{}
  for n,rowx in pairs(sort(map(rows, proj),lt"dist")) do 
    if n<#rows//2 then mid, midc = rowx.row, rowx.x end
    push(n <= #rows//2 and lefts or rights, rowx.row) end
  return {lefts=lefts, rights=rights, mid=mid,
          left =left,  right =right,  c=c} end

function DATA.tree(i,  rows,stop)
   rows = rows or self.rows
   stop = stop or rows^the.min
   local node = self:half(i,self.rows)
   if #nodes.lefts  > stop then nodes.lefts  = i:tree(nodes.lefts, stop) end
   if #nodes.rights > stop then nodes.rights = i:tree(nodes.rights,stop) end
   return node end
----------------------------------------------------------------------------------------------------
function any(t) return t[math.random(#t)] end
function many(t,n)
  local t={}; for i=1,n do push(t,any(t)) end; return t end

-- ## thing to string
function o(t,    seen)
  if type(t)~="table" then return tostring(t) end
  seen = seen or {}
  if seen[t] then return "..." end
  seen[t] = t
  local pre,fun,isPublic,o1 = t._is and  t._is or ""
  function o1(x) seen[t]=t; return o(x,seen) end
  function isPublic(k) return tostring(k):sub(1,1)~="_" end
  function fun(k,v) if isPublic(k)  then return string.format(":%s %s", k, o1(v)) end end
  return pre.."{"..table.concat(#t>0 and map(t,o1) or sort(kap(t,fun))," ").."}" end

-- ## Strings to things
function coerce(s) --> any; return int or float or bool or string from `s`
  local function word(s)
    if s=="true" then return true elseif s=="false" then return false else return s end end
  return math.tointeger(s) or tonumber(s) or word(s:match"^%s*(.-)%s*$") end

function csv(sFile,fun) --> nil; call `fun` on rows (after coercing cell text)
  local src,s,t  = io.input(sFile)
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
function lt(x) 
  return function(a,b) return a[x] < b[x] end end

function sort(t,fun) --> t; returns `t` sorted by `fun` 
  table.sort(t,fun) return t end 

function push(t,x) t[1+#t]=x; return x end

function map(t,fun) --> t; returns copy of `t`, all items filtered by `fun`.
  local u={}; for _,x in pairs(t) do push(u,fun(x)) end; return u end

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
function eg.num() oo(NUM():add(20)) end
function eg.cols() oo(COLS{"a","b","D","EX","F+"}) end 
-----------------------------------------------------------------------------------------------------
the = settings(the,help)
eg[the.go]()
for k,v in pairs(_ENV) do if not b4[k] then print("?",k,type(v)) end end
