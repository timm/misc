local help = [[
acquire.lua : row/column clustering for rep grids
(c)2022, Tim Menzies <timm@ieee.org>, BSD-2 

Reads a csv file where row1 is column names and col1,coln
is dimension left,right and all other cells are numerics.

USAGE:   lua acquire.lua  [OPTIONS]

OPTIONS:
  -d  --dump  on crash, dump stack       = false
  -f  --file  csv file                   = ../etc/data/repgrid1.csv
  -F  --Far   where to find far things   = 1
  -g  --go    start-up action            = data
  -h  --help  show help                  = false  
  -m  --min   min size for leaf clusters = .5
  -p  --p     distance coefficient       = 2
  -s  --seed  random number seed         = 937162211
  -x  --X     small x change             = .1
]]
local b4={}; for k,_ in pairs(_ENV) do b4[k] = k end
local the,coerce,kap,map,o,oo,sort,settings = {}
----------------------------------------------------------------------------------------------------
local oid,_,new,add=0
function obj(s) local x={_txt=s}; _=x; return x end
function new(kl,...)   oid=oid+1; local x={_is=kl, _id=oid}; kl.new(x,...); return x end 
function add(i,x,...)  if x ~= "?" then i.n=i.n+1;  i._is.add(i,x,...) end; return i end
----------------------------------------------------------------------------------------------------
function column(i,n,s)
  i.at, i.txt = n or 0, s or ""
  i.n=0
  i.w = i.txt:find"-$" and -1  or 1 end

local NUM = obj"NUM"
function _.new(num,n,s) 
  column(num,n,s)
  num.lo =  math.huge
  num.hi = -math.huge
  num.mu,i.m2,i.sd=0,0,0  end

function _.add(num,n,...) 
   local d = n - num.mu
   num.mu = num.mu + d/num.n
   num.m2 = num.m2 + d*(n - num.mu)
   num.sd = (num.m2 <0 or num.n < 2) and 0 or (num.m2/(num.n-1))^0.5 
   num.lo = math.min(n, num.lo)
   num.hi = math.max(n, num.hi) end 

local SYM=obj"SYM"
function _.new(sym,n,s)
  column(sym,n,s)
  sym.mode, sym.most= nil,0
  sym.has = {} end

function _.add(sym,x,inc)
  inc = inc or 1
  sym.has[x] = inc + (sym.has[x] or 0)
  if sym.has[x] > sym.most then sym.most, sym.mode = sym.has[x], x end end 
----------------------------------------------------------------------------------------------------
-- ## Strings to things
function coerce(s) --> any; return int or float or bool or string from `s`
  local function word(s)
    if s=="true" then return true elseif s=="false" then return false else return s end end
  return math.tointeger(s) or tonumber(s) or word(s:match"^%s*(.-)%s*$") end

function settings(t,s)
  s:gsub("\n[%s]+[-]([%S])[%s]+[-][-]([%S]+)[^\n]+= ([%S]+)", function(k,key,v) 
    for n,x in ipairs(arg) do
      if x=="-"..k or x=="--"..key then
        v = v=="false" and "true" or v=="true" and "false" or arg[n+1] end end
    t[key] = coerce(v) end)
  if t.help then os.exit(print(s)) end
  return t end

-- ### Lists
function sort(t,fun) table.sort(t,fun) return t end --> t; returns `t` sorted by `fun` 

function map(t,fun) --> t; returns copy of `t`, all items filtered by `fun`.
  local u={}; for __,x in pairs(t) do u[1+#u]=fun(x) end; return u end

function kap(t,fun) --> t; returns copy of `t`, all items filtered by `fun`.
  local u={}; for k,x in pairs(t) do u[1+#u]=fun(k,x) end; return u end

-- ### Printing
function oo(t) print(o(t)); return t end 
function o(t)
  if type(t)~="table" then return tostring(t) end
  local pre,fun,pre,isPublic = t._is and t._is._txt or ""
  function isPublic(k) return tostring(k):sub(1,1)~="_" end
  function fun(k,v) if isPublic(k)  then return string.format(":%s %s",k,o(v)) end end
  return pre.."{"..table.concat(#t>0 and map(t,o) or sort(kap(t,fun))," ").."}" end

oo(settings(the,help))
oo(add(new(NUM,10,"asd"),20))
for k,v in pairs(_ENV) do if not b4[k] then print("?",k,type(v)) end end
