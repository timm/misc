#!/usr/bin/env lua
--  __                 
-- /\ \                
-- \ \ \____    ___    
--  \ \ '__`\ /' _ `\  
--   \ \ \L\ \/\ \/\ \ 
--    \ \_,__/\ \_\ \_\
--     \/___/  \/_/\/_/
--

local help=[[  
bn.lua : stochastic landscape analyis
(c) 2025, Tim Menzies <timm@ieee.org> MIT license
]]

local Data = require"data"

local l = require"lib"
local abs,big,fmt,map,oo,sort  = math.abs,l.big,l.fmt,l.map,l.oo,l.sort

--## Config-----------------------------------------------------------------------------
local the={
  p    = 2,
  seed = math.randomseed(1234567891),
  file = "../../moot/optimize/misc/auto93.csv"
}

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

--## Start -----------------------------------------------------------------------------
if debug.getinfo(1, "S").short_src == arg[0] then
  for _,s in pairs(arg) do
    if egs[s] then
      egs[s](help) end end end

return {Data=Data,the=the,help=help}
