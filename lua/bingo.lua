--## Config-----------------------------------------------------------------------------
local the={
  p    = 2,
  bins = 5, 
  dims = 5,
  seed = 1234567891,
  file = "../../moot/optimize/misc/auto93.csv"
}

--## Lib -----------------------------------------------------------------------------
--### Short-cuts
local o
local big  = 1E32
local abs  = math.abs
local log  = math.log
local max  = math.max
local min  = math.min
local fmt  = string.format
local oo   = function(t) print(o(t)); return t end
local push = function(t,x)   t[1+#t]=x; return x end
local sort = function(t,fun) table.sort(t,fun); return t end
local new  = function(kl,t)  kl.__index=kl; return setmetatable(t,kl) end

local function map(t,fun,    u)
  u={}; for _,v in pairs(t) do u[1+#u] = fun(v) end; return u end

--### Strings to Things
local function atom(s,    fun) 
  function fun(s) return s=="true" or s~="false" and s end
  return math.tointeger(s) or tonumber(s) or fun(s:match"^%s*(.-)%s*$") end

local function atoms(s,    t)
  t={}; for s1 in s:gmatch("([^,]+)") do t[1+#t]=atom(s1) end; return t end

--### Thing to Strings
function o(x,      t,LIST,DICT)
  t = {}
  LIST = function() for _,v in pairs(x) do t[1+#t]=o(v) end end
  DICT = function() for k,v in pairs(x) do t[1+#t]=fmt(":%s %s",k,o(v)) end end
  if type(x) == "number" then return fmt(x//1 == x and "%s" or "%.3g",x) end
  if type(x) ~= "table" then return tostring(x) end
  if #x>0 then LIST() else DICT(); table.sort(t) end
  return "{" .. table.concat(t, " ") .. "}" end

function table.equals(t1, t2)
  if #t1 ~= #t2 then return false end
  for i = 1, #t1 do
    if t1[i] ~= t2[i] then return false end end
  return true end
--## Create ---------------------------------------------------------------------------
---- add sym and num here. handle "?" un sym num, not in data. all x y
local Data = {}

function Data:new() 
  return new(Data,{rows={}, hi={}, lo={}, x={}, y={}}) end

function Data:read(file,    src,s)
  src = io.input(file)
  while true do
    s = io.read()
    if s then self:add(atoms(s)) else io.close(src); return self end end end

function Data:add(t) 
  if #self.x==0 then self:top(t) else self:data(t) end
  return self end

function Data:top(t)
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
function Data:project(t,a,b)
  D = function(r1,r2) return self:xdist(r1,r2) end
  c = self:xdist(a,b)
  return c==0 and 0 or (D(t,a)^2 + c^2 - D(t,b)^2) / (2*c*c) end

function Data:bucket(t,a,bt)
  return min(the.bins - 1, (self:project(t,a,b) * the.bins) // 1) end

function neighbors(buckets,d,max,     out,_go)
  out = {}
  function _go(pos, idx)
    if idx > d then
      if not table.equals(pos, buckets) then
        table.insert(out, pos) end
      return end
    for delta = -1, 1 do 
      local pos1 = {table.unpack(pos)}
      pos1[idx] = pos1[idx] + delta
      if pos1[idx] >= 0 and pos1[idx] < max then
        _go(pos1, idx + 1) end end end
  _go(buckets, 1)
  return out
end

--
--## Examples -----------------------------------------------------------------------------
eg={}

eg["-h"] = function(_) oo(the) end

eg["-s"] = function(s) math.randomseed(s); the.seed=s end

eg["--neigh"] = function(_)
   oo(neighbors({3,3,3},3,4))
   oo(neighbors({3,3,3},3,3))
  end

eg["--data"] = function(_,    d) 
  d = Data:new():read(the.file)
  oo{lo=d.lo, hi=d.hi}  end

eg["--ydist"] = function(_)
  d = Data:new():read(the.file)
  oo(sort(map(d.rows, function(r) return d:ydist(r) end))) end

eg["--xdist"] = function(_)
  d = Data:new():read(the.file)
  oo(sort(map(d.rows, function(r) return d:xdist(r, d.rows[1]) end))) end
   
--## Start-up -----------------------------------------------------------------------------
math.randomseed(the.seed)
for i,s in pairs(arg) do
  if eg[s] then
    eg[s](arg[i+1] and atom(arg[i+1])) end end
