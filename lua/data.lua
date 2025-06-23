--  ____                 __                
-- /\  _`\              /\ \__             
-- \ \ \/\ \     __     \ \ ,_\     __     
--  \ \ \ \ \  /'__`\    \ \ \/   /'__`\   
--   \ \ \_\ \/\ \L\.\_   \ \ \_ /\ \L\.\_ 
--    \ \____/\ \__/.\_\   \ \__\\ \__/.\_\
--     \/___/  \/__/\/_/    \/__/ \/__/\/_/
--

local Data = {}

local lib   = require"lib"
local atoms,big   = lib.atoms, lib.big
local max,min,new,push = math.max, math.min, lib.new, lib.push

function Data:new()
  return new(Data,{is=Data, rows={}, names={}, hi={}, lo={}, x={}, y={}}) end

function Data:clone(rows,    d)
  d = Data:new():add(self.names)
  for t in pairs(rows or {}) do self:add(t) end
  return d end

function Data:read(file,    n,src,s)
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

return Data
