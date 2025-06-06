local the={p=2, Few=32, Stop=4, seed=1234567891, file="big.csv"}
math.randomseed(the.seed)

local push = table.insert
local map = function(t,f,u) u={}; for i,v in ipairs(t) do u[i]=f(v,i) end; return u end
local sum = function(t,f) local n=0; for _,v in ipairs(t) do n=n+f(v) end; return n end
local any = function(t) return t[math.random(#t)] end

local function atom(x)
  local n = tonumber(x)
  if n then return n end
  if x == "true" then return true end
  if x == "false" then return false end
  return x
end

local function split(s)
  local t = {}; for x in s:gmatch("([^,]+)") do push(t, atom(x)) end; return t
end

local function lines(file)
  local out = {}
  for line in io.lines(file) do push(out, line) end
  return out
end

local Data = {}
function Data:new()
  return setmetatable({rows={},names={},lo={},hi={},xcols={}}, {__index=self})
end

function Data:add(t)
  if #self.names==0 then
    self.names=t
    for i,name in ipairs(t) do
      if not name:match("[+-]$") then self.xcols[i]=true end
    end
  else
    push(self.rows,t)
    for i,x in ipairs(t) do
      if self.xcols[i] then
        self.lo[i] = self.lo[i] and math.min(self.lo[i],x) or x
        self.hi[i] = self.hi[i] and math.max(self.hi[i],x) or x
      end
    end
  end
  return self
end

function Data:norm(c,x)
  return (x - self.lo[c]) / ((self.hi[c] - self.lo[c]) + 1e-32)
end

function Data:xdist(t1,t2)
  local d,n=0,0
  for c in pairs(self.xcols) do
    d = d + math.abs(self:norm(c,t1[c]) - self:norm(c,t2[c]))^the.p
    n = n + 1
  end
  return (d/n)^(1/the.p)
end

function kpp(data,k,rows)
  k=k or the.Stop; rows=rows or data.rows
  local centroids = {any(rows)}
  local some = {}
  for i=1,math.min(the.Few,#rows) do some[i]=rows[i] end
  for i=2,k do
    local dists = map(some, function(x)
      return math.min(table.unpack(map(centroids, function(y)
        return data:xdist(x,y)^2 end))) end)
    local r = math.random() * sum(dists, function(z) return z end)
    for j=1,#dists do
      r = r - dists[j]
      if r <= 0 then
        push(centroids, table.remove(some,j))
        break
      end
    end
  end
  return centroids
end

-- Main
local d = Data:new()
local first = true
for _, line in ipairs(lines(the.file)) do
  local row = split(line)
  d:add(row)
end

kpp(d) -- Suppressed output for benchmarking

