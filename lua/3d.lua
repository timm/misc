#!/usr/bin/env lua
local the = {bins=10, seed=10, p=2,
             file="../../moot/optimize/misc/auto93.csv"}

local BIG = 1E32
math.randomseed(the.seed)

local any,atom,cat,fmt,max,min,new,trim,split

fmt = string.format

function any(t)     return t[ math.random(#t) ] end
function atom(s)    return tonumber(s) or trim(s) end
function max(x,y)   return x >= y and x or y end
function min(x,y)   return x <= y and x or y end
function new(kl,t)  kl.__index=kl; setmetatable(t,kl); return t end
function sort(t,f)  table.sort(t,f); return t end
function trim(s)    return s:match("^%s*(.-)%s*$") end

function split(s,   t) 
  t={}; for s1 in s:gmatch("([^,]+)") do t[1+#t]=atom(s1) end; return t end

local _kat,_cat
function cat(x)
  if type(x)=="table" then return (#x==0 and _kat or _cat)(x,{}) end
  if type(x)=="number" and x//1 ~= x then return fmt("%3g",x) end 
  return tostring(x)  end 

function _kat(t,u, _show)
  _show = function(s) return not tostring(s):find"^_" end
  for k,v in pairs(t) do if _show(k) then u[1+#u]=fmt(":%s %s",k,cat(v)) end end
  return '{' .. table.concat(sort(u)," ") .. '}' end

function _cat(t,u)
  for k,v in pairs(t) do u[1+#u] = cat(v) end 
  return '{' .. table.concat(u,", ") .. '}' end


-------------------------------------------------------------------------------
local Sym,Data,SYM,DATA = {},{}

function SYM() return new(Sym, {has={}, mode=nil, most=0}) end

function Sym.add(i,x,  n)
  i.has[x] = (n or 1) + (i.has[x] or 0)
  if i.has[x] > i.most then i.most,i.mode = i.has[x],x end end

function DATA(names,    i) 
  i = new(Data,{lo={}, hi={},  names=names, _rows={}, x={}, y={}}) 
  for c,s in pairs(names) do
    if not s:find"X$" then
      if s:find"^[A-Z]" then i.lo[c], i.hi[c] = BIG, -BIG end
      (s:find"[!+-]$" and i.y or i.x)[c] = c 
      if s:find"-$" then i.y[c] = 0 end
      if s:find"+$" then i.y[c] = 1 end end end
  return i end

function Data.norm(i,c, x) 
  return x=="?" and x or (x - i.lo[c]) / (i.hi[c] - i.lo[c]+1/BIG) end

function Data.dist(i,x,y,    d,n,_dist)
  _dist = function(c,a,b)
    if a=="?" and b=="?" then return 1 
    else if not i.lo[c] then return a==b and 0 or 1 
    else a,b = i:norm(c, a), i:norm(c, b)
         a = a == "?" and a or (b > 0.5 and 0 or 1)
         b = b == "?" and b or (a > 0.5 and 0 or 1)
         return math.abs(a - b) end end end 
  d,n = 0,0
  for c in pairs(i.x) do
    n = n + 1
    d = d + _dist(c, x[c], y[c]) ^ the.p end
  return (d/n) ^ (1/the.p) end

function Data.add(i,row)
  i._rows[1 + #i._rows] = row
  for c,x in pairs(row) do
    if x ~= "?" and i.lo[c] then
      i.hi[c] = math.max(x, i.hi[c])
      i.lo[c] = math.min(x, i.lo[c]) end end end  

function main(file,   d,t)
  for line in io.lines(file) do
    if d then d:add(split(line)) else d = DATA(split(line)) end end
  return d end
 
local function poles(a)
   local far, _rows = -1, {}
   for r in pairs(D) do
      table.insert(rows, r)
      if #rows > 20 then break end
   end
   for _, r in ipairs(rows) do
      for _, r2 in ipairs(rows) do
        if r > r2 then
           local d = dist(r, r2)
           if d > far then
              far, a[1], a[2] = d, r, r2
           end
        end
      end
   end
   far = -1
   for _, r in ipairs(rows) do
      local d = dist(r, a[1]) + dist(r, a[2])
      if d > far then
        far, a[3] = d, r
      end
   end
   far = -1
   for _, r in ipairs(rows) do
      local d = dist(r, a[1]) + dist(r, a[2]) + dist(r, a[3])
      if d > far then
        a[4] = r
      end
   end
end

d=main(arg[1] or the.file)
print(cat(d))
