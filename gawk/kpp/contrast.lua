--        _   _   ._   _|_  ._   _.   _  _|_ 
--       (_  (_)  | |   |_  |   (_|  _>   |_ 

local l=require"lib"
local the=require"about"
local kseed=require("kseed")

local keysort = l.keysort

local Data,Num,Sym = kseed.Data, kseed.Num, kseed.Sym

function Sym:bin(x,_)
  return x end

function Num:bin(x,bins) 
  if x ~= "?" then  (x - self.lo) / (self.hi - self.lo) * bins //1 end end

function Sym:merge(other)
  i,j,k = self,other,Sym:new(self.txt, self.pos)
  for _,has in pairs{i.has, j.has} do
    for x,n in pairs(has) do
      k:add(x,n)
  return k end

function Sym:merged(other,  tiny)
  i, j, k = self, other, self:merge(other)
  if i.n < tiny or j.n < tiny then return k end
  if k:ent() <= (i.n * i:ent() + j.n * j:ent()) / k.n then return k end end


function Data:contrast(rows,bins) 
  ADD = function(b,x,y)
          b.lo = math.min(b.lo, x)
          b.hi = math.max(b.hi, x)
          b.y:add(y) end
  DIV = function(col,rowss,t)
          for y,rows in pairs(rowss) do
            for _,row in pairs(rows) do
              local x,k
              x = row[col.pos]
              if x ~= "?" then
                k = col:bin(x, bins)
                t[k] = t[k] or {lo=x, hi=x, y=Sym:new(col.txt, col.pos)}
                ADD(t[k], x, y) end end end 
        end 
          
  data = Data:new(file)
  Y    = function(row) return data:ydist(row) end
  rows = data:around(the.samples)
  bestr, restr = split(keysort(rows,Y), (#rows)^0.5)
  for _,col in pairs(data.cols.x) do
    DIV(col, {best=bestr, rest=restr}, {}) end    
end

require  {Data=Data, Num=Num, Sym=Sym}
