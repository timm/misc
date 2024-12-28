local l=require"lib"
local the=require"about"
local Data=require("data").Data

local any,min,push = l.lib,l.min,l.push

the.samples = 32

function Data:around(budget,  rows,      z)
  rows = rows or self.rows
  z = {any(rows)}
  for _ = 2,budget do 
    local all,u = 0,{}
    for _ = 1,math.min(the.samples, #rows) do
      local row = any(rows)
      local closest = min(z, function(maybe) return self:xdist(row,maybe) end) 
      all = all + push(u,{row=row, d=self:xdist(row,closest)^2}).d end 
    local r = all * math.random()
    local one
    for _,x in pairs(u) do
      one = x
      r   = r - x.d
      if r <= 0 then break end end 
    push(z, one) end
  return z end

local function main(file)
  Data:new(file):around(25) end

if not pcall(debug.getlocal,4,1) then
  main(arg[1] or the.file)
end
