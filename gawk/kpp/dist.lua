--     _|  o   _  _|_   _.  ._    _   _  
--    (_|  |  _>   |_  (_|  | |  (_  (/_ 

local l = require"lib"
local the = require"about"
local data = require"data"

local Data, Num, Sym = data.Data, data.Num, data.Sym

the.p=2

function Sym:dist(p,q)
  return (p=="?" and q=="?" and 1) or (p==q and 0 or 1) end

function Num:dist(p,q)
  if (p=="?" and q=="?") then return 1 end 
  p,q = self:norm(p), self:norm(q)
  p = p ~= "?" and p or (q<0.5 and 1 or 0)
  q = q ~= "?" and q or (p<0.5 and 1 or 0)
  return math.abs(p - q) end

function Data:xdist(row1,row2,      d)
  d=0; for _,col in pairs(self.cols.x) do
         d = d + col:dist(row1[col.pos], row2[col.pos])^the.p end
  return (d / #self.cols.x) ^ (1/the.p) end

function Data:ydist(row,      d)
  d=0; for _,col in pairs(self.cols.y) do
         d = d + (col:norm(row[col.pos]) - col.goal)^the.p end
  return (d / #self.cols.y) ^ (1/the.p) end


return {Data=Data,Num=Num,Sym=Sym}
