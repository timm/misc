#!/usr/bin/env lua
local the = {seed=1234567891, goal=1, leaf=2,
             file="../../moot/optimize/misc/auto93.csv"}

local csv, pat, push,sort -- defined later
local is_num,is_y,add,add1, Sym,Num,Cols,Data ----------------------------------
function is_num(s) return s:find"^[A-Z]" end
function is_y(s)   return s:find"[+-!]$" end

function Sym(i,s) return {at=i or 0, txt=s or "", n=0, has={}} end
function Num(i,s) return {at=i or 0, txt=s or "", n=0, has={}, nump=true,
                          lo=1e32, hi=-1e32,
                          best=(s or ""):find"-$" and 0 or 1} end

function Cols(names,    cols,col)
  cols = {names=names, all={}, x={}, y={}}
  for i,s in pairs(names) do 
    col = (is_num(s) and Num or Sym)(i,s)
    push(cols.all, col)
    push(is_y(s) and cols.y or cols.x, col) end
  return cols end 

function Data(src)
  return adds({rows={},cols=nil}, src) end

function adds(data, src)
  if   type(src)=="string" 
  then for   row in csv(src)   do add(data,row) end 
  else for _,row in pairs(src) do add(data,row) end 
  end 
  for _,col in pairs(data.cols.all) do
    if col.nump then 
      col.has = sort(col.has) 
      col.lo, col.hi = col.has[1], col.has[#col.has] end end
  return data end
 
function add(data,row)
  if   data.cols 
  then for _,col in pairs(data.cols.all) do add1(col, row[col.at]) end
  else data.cols = Cols(row) end end

function add1(col,v)
  if v ~= "?" then
    col.n = col.n + 1
    col.has[1+#col.has] = v end end

function clone(data, rows)
  return adds(Data{data.cols.names}, rows or {}) end

function norm(col,n) return (n - col.lo) / (col.hi - col.lo + 1e-32) end

function disty(data,row,    n,d)
  n,d=0,0
  for _,col in pairs(data.cols.y) do 
    n = n + 1
    d = d + (norm(col,row[col.at) - col.best)^2 end
  return (d/n) ^ 0.5 end

function threshold(data)
  y= function(r) return disty(data,r) end 
  t= sort(data.rows, function(r1,r2) return y(r1) < y(r2) end)
  return y( t[((#t)^.5)//1] ) end

function bestrest(data)
  cut=threshold(data)
  good,bad={},{}
  for _,row in data.rows do push(disty(data,row) <= cut and good or bad) end
  cols1,cols2=  clone(data,good).cols, clone(data,bad).cols
  for x,col in pairs(cols1.x) do
    print(col.at, col.txt, irq(col1, cols2[x])) end end

function irq(col1,col2,    a,b,n)
  a,b = col1.has, col2.has
  n = #a // 4
  return 0.5 - (bpos(b,a[3*n]) - bpos(b,a[n])) / #b end

local fmt,lat,dat,cat -----------------------------------------
fmt=string.format

function pat(t) print(cat(t)); return t end

function cat(t)
  if type(t)~="table"  then return tostring(t) end
  if type(t)=="number" then return fmt(t % 1==0 and "%d" or "%.3f",t) end
  return "{"..table.concat(#t>0 and lat(t,{}) or sort(dat(t,{}))," ").."}" end

function lat(t, u) 
  for i,v in pairs(t) do u[i]=cat(v) end; return u end 

function dat(t, u) 
  for k,v in pairs(t) do u[1+#u] = fmt(":%s %s",k,cat(v)) end; return u end 

local known,at,map,shuffle -------------------------------------------------------------
function push(t,x) t[1+#t]=x; return x end
function sort(t,fn) table.sort(t,fn); return t end
function at(n) return function(t) return t[n] end end

function map(t,fn,  u) 
  u={};for i,v in pairs(t) do u[1+#u]=fn(v)end;return u end 

function shuffle(t)
  for i=#t,2,-1 do local j=math.random(i); t[i],t[j] = t[j],t[i] end; return t end

function bpos(a,n,   lo,hi,mid)
  lo, hi = 1, #a
  while lo < hi do
    mid = (lo + hi) // 2
    if a[mid] < n then lo = mid + 1 else hi = mid end end
  if lo > 1 and math.abs(a[lo-1]-n) < math.abs(a[lo]-n) then return lo-1 end
  return lo end

local is,cells -----------------------------------------------------------------
function is(s) return tonumber(s) or s end
function cells(s,  t)
  t={}; for s1 in s:gmatch"([^,]+)" do t[1+#t]=is(s1) end; return t end

function csv(file)
  local fp = file and io.open(file) or io.input()
  return function() local s=fp:read(); return s and cells(s) end end

local eg = {} ------------------------------------------------------------------
eg.seed= function(x) 
  the.seed= is(x); math.randomseed(the.seed) end

eg.rand= function(_) t={10,20,30,40,50,60}; pat(shuffle(t)) end
eg.goal= function(x) the.goal= is(x) end
eg.leaf= function(x) the.leaf= is(x) end
eg.rows= function(_) pat(Data(the.file).cols.x[2]) end

local function main()
  for i,s in pairs(arg or {}) do
    local key = s:gsub("^%-+", "")  
    if eg[key] then eg[key](arg[i+1]) end end end

-------------------------------------------------------------------------------
math.randomseed(the.seed)
if arg[0]:find"irq.lua" then main() end

return {Data=Data, Num=Num, Sym=Sym, csv=csv, pat=pat, cat=cat}
