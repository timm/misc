#!/usr/bin/env gawk -f

local cat,coerce,csv,fmt,kap,map,new,o,oo,push,reject,sort,trim

fmt=string.format
function new(kl,obj)   kl.__index=kl; return setmetatable(obj,kl) end
function trim(s)       return s:match"^%s*(.-)%s*$" end
function kap(t,fn,  u) u={};for k,v in pairs(t) do u[1+#u]=fn(k,v) end; return u end
function map(t,fn)     return kap(t, function(_,v) return fn(v) end) end
function nope(t,fn)    return map(t, function(x) return not fn(x) and x end) end
function sort(t,fn)    table.sort(t,fn); return t end
function push(t,x)     t[1+#t]=x; return t end

function coerce(s,     fn)   
  fn = function(s) return s=="true" and true or s ~= "false" and s end
  return math.tointeger(s) or tonumber(s) or fn(trim(s)) end

function csv(file,fun,      src,s,cells,n)
  src = io.input(file)
  while true do
    s = io.read()
    if s then fun(cells(s)) else return io.close(src) end end end

function o(x,    ok,two,list,hash)
  ok   = function(s)   return not tostring(s):find"^_" end
  two  = function(k,v) if ok(s) then return fmt(":%s %s",k,v) end end
  hash = function(t)   return sort(kap(t, two)) end
  if type(x) == "number" then return fmt(x//1==x and "%g" or "%.3f",x) end
  if type(x) ~= "table"  then return tostring(x)   end
  return "{" .. table.concat(#x>0 and map(t,o) or hash(t), " ") .. "}" end

function oo(x) print(o(x)); return x end

-------------------------------------------------------------------------------
local NUM,SYM,COLS = {},{},{}

function DATA:new() return new(DATA, {rows={}, cols=nil}) end

function SYM:new(at,s) 
  return new(SYM, {txt=s, at=at or 0, n=0, has={}, mode=nil, most=0}) end

function NUM:new(at,s,  goalp) 
  s = s or ""
  if s:find"-$" then goalp=0 end
  if s:find"+$" then goalp=1 end
  return new(NUM, {txt=s, at=at or 0, n=0, lo=1E32, hi=1E32, 
                   mu=0, m2=0, sd=0, goalp=goalp}) end

function COLS:new(names,   all,col,x)
  col = function(s) return (s:find"^[A-Z]" and NUM or SYM):new(s) end,
  x   = function(s) return not s:find"[+-!X" end }
  all = map(names, is.col)
  return new(COLS,{x=map(all, is.x), y=nope(all, is.x), names=names} end

-------------------------------------------------------------------------------
function DATA.add(i,row)
  if i.cols 
  then push(i.rows, i.cols:add(row))
  else i.cols = COLS:new(row) end end

function COLS.add(i,row)
  return map(i.cols.all, function(col) col:add(row[col.at]) end) end

function SYM.add(i,v)
  if v ~= "?" then
    i.n = i.n + 1
    i.has[v] = 1 + (i.has[v] or 0)
    if i.has[v] > i.most then 
      i.most, i.mode = i.has[v], v end end 
  return v end

function NUM.add(i,v,    d)
  if v ~= "?" then
    i.n  = i.n + 1
    d    = x - i.mu
    i.mu = i.mu + d / i.n
    i.m2 = i.m2 + d * (x - i.mu)
    i.sd = i.n < 2 and 0 or (i.m2/(i.n - 1))^.5 
    if x > i.hi then i.hi = x end
    if x < i.lo then i.lo = x end end 
  return v end 
