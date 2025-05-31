#!/usr/bin/env lua
local help=[[  
bn.lua : stochastic landscape analyis
(c) 2025, Tim Menzies <timm@ieee.org> MIT license
]]

--## Config-----------------------------------------------------------------------------
local the={
  p    = 2,
  bins = 5, 
  dims = 5,
  seed = 1234567891,
  file = "../../moot/optimize/misc/auto93.csv"
}
math.randomseed(the.seed)

--## Lib -----------------------------------------------------------------------------
--### Short-cuts
local o
local big  = 1E32
local abs  = math.abs
local log  = math.log
local max  = math.max
local min  = math.min
local fmt  = string.format
local push = function(t,x)     t[1+#t]=x; return x                              end
local oo   = function(t)       print(o(t)); return t                            end
local any  = function(t)       return t[math.random(#t)]                        end 
local sort = function(t,fn)    table.sort(t,fn); return t                      end
local new  = function(kl,t)    kl.__index=kl; return setmetatable(t,kl)         end
local many = function(t,n,  u) u={}; for _ =1,n do u[1+#u]=any(t) end; return u end

local function map(t,fn,    u)
  u={}; for _,v in pairs(t) do u[1+#u] = fn(v) end; return u end

local function sum(t,fn,    n)
  n=0; for _,v in pairs(t) do n = n + fn(v) end; return n end

local function most(t,fn,    m,n,x)
  n = -big
  for _,v in pairs(t) do m=fn(v); if m>n then n,x=m,v end end; return x end 

local function tequals(t1, t2)
  if #t1 ~= #t2 then return false end
  for i = 1, #t1 do
    if t1[i] ~= t2[i] then return false end end
  return true end

--### Strings to Things
local function atom(s,    fn) 
  function fn(s) return s=="true" or s~="false" and s end
  return math.tointeger(s) or tonumber(s) or fn(s:match"^%s*(.-)%s*$") end

local function atoms(s,    t)
  t={}; for s1 in s:gmatch("([^,]+)") do t[1+#t]=atom(s1) end; return t end

--### Thing to Strings
function o(x,      t,ARR,DIC,NUM)
  t   = {}
  ARR = function() for _,v in pairs(x) do t[1+#t]=o(v) end end
  DIC = function() for k,v in pairs(x) do t[1+#t]=fmt(":%s %s",k,o(v)) end end
  NUM = function() return x//1 == x and "%s" or "%.3g" end
  if type(x) == "number" then return fmt(NUM(x), x) end
  if type(x) ~= "table"  then return tostring(x) end
  if #x>0 then ARR() else DIC(); table.sort(t) end
  return "{" .. table.concat(t, " ") .. "}" end

--## Create ---------------------------------------------------------------------------
local Data = {}

function Data:new() 
  return new(Data,{is=Data, rows={}, names={}, hi={}, lo={}, x={}, y={}}) end

function Data:clone(rows)
  d = Data:new():add(self.names)
  for t in pairs(rows or {}) do self:add(t) end
  return d end

function Data:read(file,    n,src)
  n,src = 0,io.input(file)
  while true do
    s = io.read()
    if s then n=n+1; self:add(atoms(s)) else io.close(src); return self end end end

function Data:add(t) 
  if #self.names==0 then self:top(t) else self:data(t) end
  return self end

function Data:top(t)
  self.names = t
  for c,s in pairs(t) do 
    if s:find"^[A-Z]" then self.lo[c], self.hi[c] = big, -big end
    if not s:find"X$" then
      if s:find"[+-]$" 
      then self.y[c] = s:find"-$" and 0 or 1 
      else self.x[c] = c end end end end

function Data:data(t)
  push(self.rows, t)
  for c,x in pairs(t) do
    if x ~= "?" and self.hi[c] then
      self.lo[c] = min(x, self.lo[c]) 
      self.hi[c] = max(x, self.hi[c]) end end end

--## Query ---------------------------------------------------------------------------
function Data:norm(c,x)
  return x=="?" and x or (x - self.lo[c]) / (self.hi[c] - self.lo[c] +1/big) end

function Data:ydist(t)
  local n,d=0,0
  for c,goal in pairs(self.y) do
    n = n + 1
    d = d + abs(self:norm(c, t[c]) - goal)^the.p end
  return (d/n) ^ (1/the.p) end

function Data:xdist(t1,t2)
  local n,d=0,0
  for c in pairs(self.x) do
    n = n + 1
    d = d + self:_xdist(c, t1[c], t2[c])^the.p end
  return (d/n) ^ (1/the.p) end

function Data:_xdist(c,u,v)
  if u=="?" and v=="?" then return 1 end
  if not self.hi[c] then return u==v and 0 or 1 end
  u= self:norm(c,u)
  v= self:norm(c,v)
  u= u ~= "?" and u or (v> .5 and 0 or 1)
  v= v ~= "?" and v or (u> .5 and 0 or 1)
  return abs(u -v) end 

--### Project
function Data:project(t,a,b,    X,c)
  X = function(r1,r2) return self:xdist(r1,r2) end
  c = self:xdist(a,b)
  return c==0 and 0 or (X(t,a)^2 + c^2 - X(t,b)^2) / (2*c*c) end

function Data:bucket(t,a,b)
  return min(the.bins - 1, (self:project(t,a,b) * the.bins) // 1) //1 end

function Data:extrapolate(t,a,b,     ya,yb)
  ya, yb = self:ydist(a), self:ydist(b)
  return ya + self:project(t,a,b) * (yb - ya) end

function Data:corners(      S,out,few)
  S   = function(x) return sum(out, function(y) return self:xdist(x,y) end) end
  few = many(self.rows,100+1)
  out = {table.remove(few,1)}
  out = {most(few,S)} -- ignore initial random point
  for i = 1,the.dims do push(out, most(few, S)) end
  return out end

function Data:buckets(crnrs,   tmp,minPt)
  tmp = {}
  for _,row in pairs(self.rows) do
    local k={}
    for i=1,#crnrs-1 do push(k, self:bucket(row, crnrs[i], crnrs[i+1])) end
    s = table.concat(k,",")
    tmp[s] = tmp[s] or {key=s, data=self:clone()}
    tmp[s].data:add(row) end
  minPt = #(data.rows) < 100 and 2 or 2*the.dims
  return map(tmp,
        function(kd) 
                                                                       proint(#(kd.data.rows));
            if #(kd.data.rows) > minPt then return kd end end) end 

function neighbors(buckets,d,max,     out,_go)
  out = {}
  function _go(pos, idx)
    if idx > d then
      if not tequals(pos, buckets) then
        table.insert(out, pos) end
      return end
    for delta = -1, 1 do 
      local pos1 = {table.unpack(pos)}
      pos1[idx] = pos1[idx] + delta
      if pos1[idx] >= 0 and pos1[idx] < max then
        _go(pos1, idx + 1) end end end
  _go(buckets, 1)
  return out end


--## Examples -----------------------------------------------------------------------------
local egs = {}

local function eg(t)
  help = help..fmt('\n   %-10s%-8s %s',t.flag,t.arg or "",t.txt) 
  egs[t.flag] = function(arg,     ok,err)
     ok,err = xpcall(function() math.randomseed(the.seed); t.fn(arg) end, debug.traceback)
     if not ok then print(">>> Error: ["..t.flag.."]", err) end 
     return ok and 0 or 1 end end

eg{flag="-h", txt="show help",       
   fn=function(_) print(help) end} 

eg{flag="-f", txt="set data file",   
   fn=function(x) the.file=x end, arg="file"}

eg{flag="-s", txt="set random seed", 
   fn=function(x) the.seed=x end, arg="seed"}

eg{flag="--the", txt="show config",  
   fn=function(_) oo(the) end}

eg{flag="--neigh", txt="report some neighbors", fn=function(_)
  oo(neighbors({3,3,3},3,5))
  oo(neighbors({3,3,3},3,5)) end}

eg{flag="--data", txt="read some data", fn=function(_,  d)
  d = Data:new():read(the.file)
  oo{lo=d.lo, hi=d.hi} end}

eg{flag="--ydist", txt="show some ydistances", fn=function(_,    d)
  d = Data:new():read(the.file)
  oo(sort(map(d.rows, function(r) return d:ydist(r) end))) end}

eg{flag="--xdist", txt="show some xdistances", fn=function(_,    d)
  d = Data:new():read(the.file)
  oo(sort(map(d.rows, 
              function(r) return d:xdist(r, d.rows[1]) end))) end}
  
eg{flag="--prjct", txt="show some projections", fn=function(_,    d,t) 
  d = Data:new():read(the.file)
  t = many(d.rows,10)
  print(d:project(t[8],t[9],t[10])) end}

eg{flag="--crnrs", txt="show some corners", fn=function(_,   d,t) 
  d = Data:new():read(the.file)
  t = d:corners() 
  for i=1,#t-1 do print(d:xdist(t[i], t[i+1])) end end}

eg{flag="--bucks", txt="show some buckets", 
  fn=function(_,   t,data1, crnrs,b) 
    data1 = Data:new():read(the.file)
    crnrs = data1:corners()
    b     = data1:buckets(crnrs)
    for _,kd in pairs(b) do oo(#(kd.data.rows)) end end}

--## Start -----------------------------------------------------------------------------
if debug.getinfo(1, "S").short_src == arg[0] then
  for i,s in pairs(arg) do
    if egs[s] then
      egs[s](arg[i+1] and atom(arg[i+1])) end end end

return {
  Data=Data, atom=atom, atoms=atoms, many=many, map=map, most=most, 
  neighbors=neighbors, new=new, o=o, oo=oo, sort=sort, sum=sum }
