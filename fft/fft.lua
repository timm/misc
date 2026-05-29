#!/usr/bin/env lua
local the,help = {}, [[
fft.lua, multi objective tree building
(c) 2025, Tim Menzies <timm@ieee.org>, MIT license

Options:
 -s --random seed   seed=1234567891
 -d --depth of tree depth=4
 -f data file       file=auto93.csv
]]

local min,max,HI = math.min. math.max, math.huge
local push,trim,coerce,dataClone, dataHeader, dataRead
local Num,Sym,Data,isSym,isData,add,adds,dataHeader

push = function(t,x) t[1+#t]=x; return x end
trim = function(s) return s:match"^%s*(.-)%s*$" end
as= function (s) 
      return s=="True" or (s~="False" and (tonumber(z) or s)) end

for k,v in help:gmatch"(%w+)=(%S+)" do the[k] = as(trim(v)) end

-----------------------------------------------------------------
function Data()      return {rows={},cols={}} end
function Sym(at,txt) return {at=at or 0,txt=txt or "",has={}} end
function Num(at,txt) return {at=at or 0,txt=txt or "",
	                     goal=(txt or ""):find"-$" and -1 or 1,
	                     lo=HI, hi=-HI, mu=0, n=0} end

function isSym(c)  return c.has  ~= nil end
function isData(c) return c.rows ~= nil end

function add(i,v,    t,d)
  if v=="?" then return v end
  if isSym(i) then i.has[v] = 1 + (i.has[v] or 0)
  elseif isData(i) then
    if next(i.cols) then
      t={}; for _,c in ipairs(i.cols.all) do 
              push(t, add(c,v[c.at])) end
      push(i.rows,t)
    else i.cols = dataHeader(v) end
  else
    i.n = i.n + 1
    d = v - i.mu
    i.mu = i.mu + d/i.n 
    i.lo = min(i.lo, v)
    i.hi = max(i.hi, v) end
  return v end

function adds(src,i)
  i = i or Num()
  if type(src)=="function" then for x in src do add(i,x) end
  else for _,x in ipairs(src or {}) do add(i,x) end end
  return i end

-----------------------------------------------------------------
function dataClone(data, rows)
  return adds(rows, adds({data.cols.names}, Data())) end

function dataHeader(names,    i,col,last)
  i = {all={}, x={}, y={}, klass=nil, names=names}
  for n,s in ipairs(names) do
    col = push(i.all, s:match"^%u" and Num(n,s) or Sym(n,s))
    if not s:find"X$" then
      if s:find"!$" then i.klass = col end
      push(s:find"[-+!]$" and i.y or i.x, col) end end
  i.klass = i.klass or i.y[1]
  return i end

function dataRead(file,    d,t)
  d = Data()
  for line in io.lines(file) do
    if line:sub(1,1) ~= "#" and line:match"%S" then
      t={}; for x in line:gmatch"[^,]+" do push(t, as(x)) end
      add(d, t) end end
  return d end
