--     |    _   _    _    _| 
--     |<  _>  (/_  (/_  (_| 

local l=require"lib"
local the=require"about"
local dist=require("dist")

local Data,Num,Sym = dist.Data, dist.Num, dist.Sym

local any, cli, min, push, shuffle = l.any, l.cli, l.min, l.push, l.shuffle

the.samples = 25

function Data:around(budget,  rows,      z)
  rows = rows or self.rows
  z = {any(rows)}
  for _ = 2,budget do 
    local all,u,row,closest
    all,u = 0,{}
    for _ = 1,math.min(the.samples, #rows) do
      row = any(rows)
      closest = min(z, function(maybe) return self:xdist(row,maybe) end) 
      all = all + push(u,{row=row, d=self:xdist(row,closest)^2}).d 
    end 
    local r = all * math.random()
    local one
    for _,x in pairs(u) do
      one = x.row
      r   = r - x.d
      if r <= 0 then break end 
    end 
    push(z, one) 
  end
  return z end

local function kseed(file,    data,Y)
  data = Data:new(file)
  Y = function(row) return data:ydist(row) end
  for _=1,50 do 
    l.shuffle(data.rows)
    print(Y( min(data:around(the.samples),Y))) end
end

if not pcall(debug.getlocal,4,1) then 
  main(cli(the).file) end

require  {Data=Data, Num=Num, Sym=Sym, kseed=kseed}
