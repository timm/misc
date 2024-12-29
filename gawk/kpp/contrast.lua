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

function Data:contrast(rows,bins) 
  ADD = function(lohiy, x, y)
          lohiy.lo = math.min( lohiy.lo, x )
          lohiy.hi = math.max( lohiy.hi, x )
          lohiy.y:add(y) end
  DIV = function(col,rowss,t)
          for y,rows in pairs(rowss) do
            for _,row in pairs(rows) do
              x = row[col.pos]
              if x ~= "?" then
                b = col:bin(x, bins)
                t[b] = t[b] or {lo=x, hi=x, y=Sym:new(col.txt, col.pos)}
                ADD(t[b], x y) end end end 
        end 
          
  data = Data:new(file)
  Y    = function(row) return data:ydist(row) end
  rows = data:around(the.samples)
  bestr, restr = split(keysort(rows,Y), (#rows)^0.5)
  for _,col in pairs(data.cols.x) do
    DIV(col, {best=bestr, rest=restr}, {}) end    
end

require  {Data=Data, Num=Num, Sym=Sym}
