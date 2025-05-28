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

--## Create ---------------------------------------------------------------------------
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


function neighbors(c, hi)
  local out = {}
  -- Local function to check if all elements are within the range [0, hi)
  local function all(t, hi)
    for _, x in ipairs(t) do
      if x < 0 or x >= hi then return false end
    end
    return true
  end
  local function go(i, p)
    p = p or {} -- This line is fine, though p should never be nil from your calls

    if i > #c then
      -- DEBUGGING ADDITIONS:
      print(string.format("DEBUG: Reached base case: i=%s, #c=%s", i, #c))
      print("DEBUG: p type:", type(p))
      if type(p) == "table" then
        print("DEBUG: p raw content (pairs):")
        for k, v in pairs(p) do
          print(string.format("  p[%s] (%s) = %s (%s)", tostring(k), type(k), tostring(v), type(v)))
        end
        print("DEBUG: p sequence content (ipairs):")
        for k, v in ipairs(p) do
          print(string.format("  p[%s] (%s) = %s (%s)", tostring(k), type(k), tostring(v), type(v)))
        end
        print(string.format("DEBUG: p[1] value: %s, type: %s", tostring(p[1]), type(p[1])))
        print(string.format("DEBUG: #p: %s", #p))
      end
      
      -- ORIGINAL LINE CAUSING ERROR:
      if table.concat(p) ~= table.concat(c) and all(p, hi) then
        table.insert(out, p)
      end
    else
      for _, d in ipairs({-1, 0, 1}) do
        -- Ensure c[i] is not nil before arithmetic, though this would be a different error
        if c[i] == nil then
            print(string.format("ERROR: c[%s] is nil!", i))
        end
        go(i + 1, {table.unpack(p), c[i] + d})
      end
    end
  end end

-- Example usage
eg = {}
eg["--neigh"] = function()
  for _, x in ipairs(neighbors({2, 2, 2}, 4)) do
    print(table.concat(x, ", "))
  end
end

-- Execute the example
eg["--neigh"]()



--## Examples -----------------------------------------------------------------------------
eg={}

eg["-h"] = function(_) oo(the) end

eg["-s"] = function(s) math.randomseed(s); the.seed=s end

eg["--neigh"] = function(_)
   for _,x in neighbors({2,2,2},4) do oo(x) end end

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
